module Auctions.Actors

open System
open System.Threading
open System.Threading.Tasks
open Commands
open Domain
open Either
open System.Collections.Generic

(*

signal -> delegator (calculate x) -> agents.[x] 

signal start auction -> 
                        agent.Start()
                        agents <- agent :: agents 
singal end auction ->
                        agent.Stop()
                        agents <- agents |> List.except [agent]

command    -> agents.[x] --> auction listeners
           |
           \-> persisters.[...] -> persist command


Assumptions:

- each auction get 1 agent 
- all commands are logged in redis, json et.c. 
- one dedicated thread per command persister (json, redis)


query      -> query agent.[y] --> Result<QueryResult,QueryError>


signal -> delegator 

 message time 

*)

type Agent<'T> = MailboxProcessor<'T>

type AuctionEnded = Auction * Bid option

type AgentSignals = 
  | AgentBid of Bid * AsyncReplyChannel<Result<unit, Errors>>
  | AuctionEnded of DateTime * AsyncReplyChannel<AuctionEnded option>
  | CollectAgent of DateTime * AsyncReplyChannel<AuctionEnded option>

//type AuctionAgent = Agent<AgentSignals>
type AuctionAgent(auction) =
  let agent = Agent<AgentSignals>.Start(fun inbox -> 
    (let validateBid = validateBid auction
     let mutable bids = []
     
     let maxBid() = 
       if List.isEmpty bids then None
       else Some(bids |> List.maxBy (fun b -> b.amount)) // we assume that we use a fixed currency
     
     /// try to signal auction ended
     let tryGetAuctionEnded time : AuctionEnded option = 
       if auction.endsAt < time then Some(auction, maxBid())
       else None
     
     let rec messageLoop() = 
       async { 
         let! msg = inbox.Receive()
         match msg with
         | AgentBid(bid, reply) -> 
           reply.Reply(either { 
                         (* 
                        We assume that you convert to VAC before sending bid to agent

                        in a future scenario we might want to add different 
                        auction type rules

                        - perhaps you cannot bid lower than the "current bid"
                        - perhaps you are forbidden from raising with to small of a sum
                          compared to the "current bid"
                        *)
                         do! validateBid bid
                         do! validateCurrency bid
                         bids <- bid :: bids
                       })
           return! messageLoop()
         | AuctionEnded(now, reply) -> 
           (*
            When the agent receives this signal 
            - it can start replying with only bid rejected response
            - make sure to send out signal about auction status (if there is a winner)
            *)
           reply.Reply(tryGetAuctionEnded now)
           return! messageLoop()
         | CollectAgent(now, reply) -> 
           (*
            When the agent receives this signal
            - it should collect any dangling business rules
                - for instance send out auction end signals 
            - quit
            *)
           reply.Reply(tryGetAuctionEnded now)
           return ()
       }
     
     messageLoop()))

  member this.AgentBid bid = agent.PostAndAsyncReply(fun reply -> AgentBid(bid, reply))
  member this.AuctionEnded time = agent.PostAndAsyncReply(fun reply -> AuctionEnded(time, reply))
  member this.Collect time = agent.PostAndAsyncReply(fun reply -> CollectAgent(time, reply))


let createAgent auction = AuctionAgent auction
 
type DelegatorSignals = 
  /// From a user command (i.e. create auction or place bid) you expect either a success or an error
  | UserCommand of Command * AsyncReplyChannel<Result<CommandSuccess, Errors>>
  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time 
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp
  /// Do we need to do anything if we have a handled shutdown of the delegator?
  | CollectDelegator

type AuctionDelegator (r) = 
  let agent =Agent<DelegatorSignals>.Start(fun inbox -> 
     let mutable activeAuctions = r |> Repo.auctions |> List.filter (Auction.hasEnded DateTime.UtcNow)
     let agents = Dictionary<AuctionId, AuctionAgent>()
     for auction in activeAuctions do
        agents.Add( auction.id, createAgent auction)

     let userCommand cmd now (reply:AsyncReplyChannel<Result<CommandSuccess, Errors>>)=
       async{
         let auctionHasEnded = Auction.hasEnded now
         match cmd with
         | AddAuction(at, auction) -> 
           if not (auctionHasEnded auction) then
             agents.Add(auction.id, createAgent auction)
             activeAuctions <- auction :: activeAuctions
             reply.Reply(Ok (AuctionAdded(at, auction)))
           else reply.Reply(Error (AuctionHasEnded auction.id))
         | PlaceBid(at, bid) -> 
           let auctionId = Command.getAuction cmd
           match agents |> Dic.tryGet auctionId with
           | Some auctionAgent -> let! m = auctionAgent.AgentBid bid
                                  reply.Reply(m |> Result.map (fun () -> BidAccepted(at, bid)))
           | None -> reply.Reply(Error (AuctionNotFound bid.auction))
       }

     let wakeUp now= 
       let auctionHasEnded = Auction.hasEnded now

       let (hasEnded, isStillActive) = activeAuctions |> List.partition auctionHasEnded
       async {
         for auction in hasEnded do 
             do! agents
                |> Dic.tryGet auction.id 
                |> function 
                  | Some agent -> 
                      async{
                        let! res =agent.AuctionEnded now
                        match res with 
                            | Some auctionHasEnded-> () // do stuff!
                            | None->()
                      }

                  | None -> async { return () }
         activeAuctions <- isStillActive
       }

     let collect now =
       async{
         for agent in agents.Values do 
             let! res = agent.Collect now
             match res with
                 | Some auctionHasEnded-> () // what should you do here? 
                 | None->()
       }

     let rec messageLoop() = 
       async { 
         let! msg = inbox.Receive()
         let now = DateTime.UtcNow
         match msg with
         | UserCommand(cmd, reply) -> 
           do! userCommand cmd now reply
           return! messageLoop()
         | WakeUp -> 
           do! wakeUp now
           return! messageLoop()
         | CollectDelegator -> 
           do! collect now
       }
     messageLoop())
  member this.UserCommand cmd = agent.PostAndAsyncReply(fun reply -> UserCommand(cmd, reply))
  member this.WakeUp time = agent.Post WakeUp 
  member this.Collect time = agent.Post CollectDelegator

let createAgentDelegator r = AuctionDelegator r