module Auctions.Actors

open System
open Commands
open Domain
open System.Collections.Generic

type Agent<'T> = MailboxProcessor<'T>

type AuctionEnded = Auction * (Amount * User) option

type AgentSignals = 
  | AgentBid of Bid * AsyncReplyChannel<Result<unit, Errors>>
  | GetBids of AsyncReplyChannel<Bid list>
  | AuctionEnded of DateTime * AsyncReplyChannel<AuctionEnded option>
  | CollectAgent of DateTime * AsyncReplyChannel<AuctionEnded option>

type AuctionAgent(auction, bids) =
  let agent = Agent<AgentSignals>.Start(fun inbox -> 
    (let validateBid = validateBid auction
     let validateBidForAuctionType = validateBidForAuctionType auction 
     let mutable bids = bids
     
     /// try to signal auction ended
     let tryGetAuctionEnded time : AuctionEnded option = 
       if auction.endsAt < time then Some(auction, (Auction.getAmountAndWinner auction bids time))
       else None
     
     let rec messageLoop() = 
       async { 
         let! msg = inbox.Receive()
         match msg with
         | AgentBid(bid, reply) -> 
           reply.Reply(result { 
                         do! validateBid bid
                         do! validateBidForAuctionType bids bid
                         bids <- bid :: bids
                       })
           return! messageLoop()
         | GetBids reply->
           reply.Reply bids
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
  member this.GetBids () = agent.PostAndAsyncReply(fun reply -> GetBids(reply))
  member this.AuctionEnded time = agent.PostAndAsyncReply(fun reply -> AuctionEnded(time, reply))
  member this.Collect time = agent.PostAndAsyncReply(fun reply -> CollectAgent(time, reply))


let createAgent auction bids = AuctionAgent (auction,bids)
type AuctionAndBids = Auction * (Bid list)
type DelegatorSignals = 
  /// From a user command (i.e. create auction or place bid) you expect either a success or an error
  | UserCommand of Command * AsyncReplyChannel<Result<CommandSuccess, Errors>>
  | GetAuction of AuctionId *AsyncReplyChannel<AuctionAndBids option>
  | GetAuctions of AsyncReplyChannel<Auction list>

  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time 
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp
  /// Do we need to do anything if we have a handled shutdown of the delegator?
  | CollectDelegator

type AuctionDelegator(r, persistCommand) = 
  let agent =Agent<DelegatorSignals>.Start(fun inbox -> 
     let mutable auctions = r |> Repo.auctions |> List.map (fun a->a.id,a) |> Map
     let mutable activeAuctions = r |> Repo.auctions |> List.filter (not<< Auction.hasEnded DateTime.UtcNow)
     
     let agents = Dictionary<AuctionId, AuctionAgent>()
     for auction in activeAuctions do
        agents.Add( auction.id, createAgent auction (r.GetBidsForAuction auction.id))

     let userCommand cmd now (reply:AsyncReplyChannel<Result<CommandSuccess, Errors>>)=
       async{
         let auctionHasEnded = Auction.hasEnded now
         match cmd with
         | AddAuction(at, auction) -> 
           if not (auctionHasEnded auction) then
             agents.Add(auction.id, createAgent auction [])
             auctions <- auctions.Add (auction.id, auction)
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
           do persistCommand cmd
           do! userCommand cmd now reply
           return! messageLoop()
         | GetAuction (auctionId,reply) ->
            do! (agents |> Dic.tryGet auctionId , auctions.TryFind auctionId)
              |> function 
                  | Some agent, Some auction -> 
                    async{
                      let! bids= agent.GetBids()
                      reply.Reply (Some(auction,bids))
                    }
                  | None,None  -> async{ reply.Reply None }
                  | None,Some auction-> async { reply.Reply (Some(auction,[])) } //TODO
                  | Some agent,None-> failwith "An agent exists without there being an auction"
            return! messageLoop()
            //reply.Reply( auctions |> List.tryFind (fun a->a.id=auctionId) )
         | GetAuctions (reply) ->
            reply.Reply (auctions |> Map.toList |> List.map snd)
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
  member this.GetAuctions ()= agent.PostAndAsyncReply(fun reply -> GetAuctions(reply))
  member this.GetAuction auctionId= agent.PostAndAsyncReply(fun reply -> GetAuction(auctionId,reply))

let createAgentDelegator r = AuctionDelegator r