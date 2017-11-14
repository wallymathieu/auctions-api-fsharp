module Auctions.Actors
open Auctions.Domain

open System
open System.Collections.Generic

type Agent<'T> = MailboxProcessor<'T>
type AuctionEnded = (Amount * User) option

type AgentSignals = 
  | AgentBid of Bid * AsyncReplyChannel<Result<unit, Errors>>
  | GetBids of AsyncReplyChannel<Bid list>
  | AuctionEnded of DateTime * AsyncReplyChannel<AuctionEnded>
  | HasEnded of DateTime * AsyncReplyChannel<Boolean>
  | CollectAgent of DateTime * AsyncReplyChannel<AuctionEnded>

type AuctionAgent(auction, state:S) =
  let agent = Agent<AgentSignals>.Start(fun inbox -> 
    (let validateBid = fun b->Auction.validateBid b auction
     let mutable state = state

     let rec messageLoop() = 
       async { 
         let! msg = inbox.Receive()
         match msg with
         | AgentBid(bid, reply) -> 
           reply.Reply(result { 
                         do! validateBid bid
                         let (next,res)=S.addBid bid state
                         state <- next
                         return! res
                       })
           return! messageLoop()
         | GetBids reply->
           let bids = S.getBids state
           reply.Reply bids
           return! messageLoop()
         | AuctionEnded(now, reply) -> 
           (*
            When the agent receives this signal 
            - it can start replying with only bid rejected response
            - make sure to send out signal about auction status (if there is a winner)
            *)
           state <- S.inc now state
           reply.Reply(S.tryGetAmountAndWinner state)
           return! messageLoop()
         | CollectAgent(now, reply) -> 
           (*
            When the agent receives this signal
            - it should collect any dangling business rules
                - for instance send out auction end signals 
            - quit
            *)
           state <- S.inc now state
           reply.Reply(S.tryGetAmountAndWinner state)
           return ()
         | AgentSignals.HasEnded (now,reply)->
           state <- S.inc now state
           reply.Reply(S.hasEnded state)
           return! messageLoop()
       }
     
     messageLoop()))

  member this.AgentBid bid = agent.PostAndAsyncReply(fun reply -> AgentBid(bid, reply))
  member this.GetBids () = agent.PostAndAsyncReply(fun reply -> GetBids(reply))
  member this.AuctionEnded time = agent.PostAndAsyncReply(fun reply -> AuctionEnded(time, reply))
  member this.Collect time = agent.PostAndAsyncReply(fun reply -> CollectAgent(time, reply))
  member this.HasEnded time = agent.PostAndAsyncReply(fun reply -> AgentSignals.HasEnded(time, reply))

/// repository that takes commands and translate them to auctions and auction states
/// assumption is that it's used in a single threaded sync manner
type private Repository ()= 
  let auctions=Dictionary<AuctionId,Auction*S>()

  member this.Auctions() : (Auction*S) list = 
    auctions.Values |> Seq.toList

  member this.handle = function 
    | AddAuction (time,a)->
      if not (auctions.ContainsKey a.id) then
        let empty =Auction.emptyState a
        auctions.Add ( a.id, (a,empty) )
      else
        ()
    | PlaceBid (time,b)->
      maybe { 
        let! (auction,state) = match auctions.TryGetValue b.auction with
                               | true, value -> Some(value)
                               | false, _ -> None
        match Auction.validateBid b auction with
        | Ok _ ->
          let (next,_)= S.addBid b state
          auctions.[auction.id]<- (auction,next)
        | Error _ -> ()
      } |> ignore

let createAgent auction bids = AuctionAgent (auction,bids)
type AuctionAndBidsAndMaybeWinnerAndAmount = Auction * (Bid list) * AuctionEnded
type DelegatorSignals = 
  /// From a user command (i.e. create auction or place bid) you expect either a success or an error
  | UserCommand of Command * AsyncReplyChannel<Result<CommandSuccess, Errors>>
  | GetAuction of AuctionId *AsyncReplyChannel<AuctionAndBidsAndMaybeWinnerAndAmount option>
  | GetAuctions of AsyncReplyChannel<Auction list>

  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time 
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp of AsyncReplyChannel<unit>
  /// Do we need to do anything if we have a handled shutdown of the delegator?
  | CollectDelegator
type AuctionDelegator(commands:Command list, persistCommand, now) = 
  let agent =Agent<DelegatorSignals>.Start(fun inbox -> 
    let mutable agents = let _now =now()
                         let r = Repository()
                         List.iter r.handle commands
                         r.Auctions()
                         |> List.map (fun (auction,state)->
                                        let next =S.inc _now state
                                        auction.id, 
                                        if not (S.hasEnded next)
                                        then auction,Choice1Of2 (createAgent auction next)
                                        else auction,Choice2Of2 (S.tryGetAmountAndWinner next, S.getBids next)
                                      )
                         |> Map

    /// Note, will mutate agents map
    let tryFindTuple_mut auctionId=
      let now = now()
      async{
      match agents |> Map.tryFind auctionId with
      | Some (auction, Choice1Of2 auctionAgent) as v -> 
        let! hasEnded = auctionAgent.HasEnded(now)
        if hasEnded then
          let! bids = auctionAgent.GetBids()
          let! ended= auctionAgent.Collect now
          let value =(auction,Choice2Of2 (ended,bids))
          agents <- agents.Add(auction.id, value)
          return Some value
        else
          return v
      | Some v ->
        return Some v 
      | None -> 
        return None
      }

    /// Note, will mutate agents map
    let tryFindAgent_mut auctionId=
      async{
      let! agent= tryFindTuple_mut auctionId
      match agent with
      | Some (auction, Choice1Of2 auctionAgent) ->
        return Ok auctionAgent
      | Some (auction, Choice2Of2 _) -> 
        return Error (AuctionHasEnded auctionId)
      | None -> 
        return Error (AuctionNotFound auctionId)
      }

    let userCommand cmd now (reply:AsyncReplyChannel<Result<CommandSuccess, Errors>>)=
       async{
         match cmd with
         | AddAuction(at, auction) -> 
           if auction.startsAt > now then
             agents <- agents.Add(auction.id, (auction ,Choice1Of2 (createAgent auction (Auction.emptyState auction))))
             reply.Reply(Ok (AuctionAdded(at, auction)))
           else reply.Reply(Error (AuctionHasEnded auction.id))
         | PlaceBid(at, bid) -> 
           let auctionId = Command.getAuction cmd
           let! maybeAgent = tryFindAgent_mut auctionId
           match maybeAgent with
           | Ok auctionAgent -> 
              let! m = auctionAgent.AgentBid bid
              reply.Reply(m |> Result.map (fun () -> BidAccepted(at, bid)))
           | Error err -> 
              reply.Reply(Error err)
       }

    let wakeUp now= 
      let hasEnded id=function
                    | Choice1Of2 (agent:AuctionAgent)-> 
                      async{
                        let! ended=agent.HasEnded(now)
                        return if ended then Some (id,agent) else None
                      }
                    | _-> async {return None}
      let hasEnded = agents 
                          |> Map.toSeq
                          |> Seq.map (fun (id, (_, c))-> hasEnded id c)
                          |> Async.Parallel


      let endAgent id (agent:AuctionAgent)= 
          async{
            let (auction,_) = Map.find id agents
            let! bids= agent.GetBids()
            let! endedAuction = agent.Collect now
            do agents <- agents.Add (auction.id,(auction,Choice2Of2 (endedAuction,bids)))
          }
      async {
        let! endedAgents =hasEnded
        for (id,agent) in endedAgents |> Array.choose id  do 
            do! endAgent id agent
      }

    let collect now = 
       async{
         for kv in agents do 
           let (a,c)= kv.Value
           match c with 
           |Choice1Of2 agent->
              do! Async.Ignore (agent.Collect now)
           |Choice2Of2 _ -> ()
       }

    let getAuction auctionId (reply:AsyncReplyChannel<AuctionAndBidsAndMaybeWinnerAndAmount option>)=
      async {
      let! t= tryFindTuple_mut auctionId
      match t with
      | None  ->  reply.Reply None 
      | Some (auction, Choice1Of2 agent) -> 
        let! bids= agent.GetBids()
        reply.Reply (Some(auction, bids, None))
      | Some (auction, Choice2Of2 (winnerAndAmount,bids)) -> 
        reply.Reply (Some(auction, bids, winnerAndAmount))
      }
    let rec messageLoop() = 
      async { 
        let! msg = inbox.Receive()
        let now = now()
        match msg with
         | UserCommand(cmd, reply) -> 
           do persistCommand cmd
           do! userCommand cmd now reply
           return! messageLoop()
         | GetAuction (auctionId,reply) ->
           do! getAuction auctionId reply
           return! messageLoop()
         | GetAuctions (reply) ->
           let auctions =agents 
                          |> Map.toList
                          |> List.map (snd>>fst)
           reply.Reply auctions
           return! messageLoop()
         | WakeUp reply -> 
           do! wakeUp now
           do reply.Reply()
           return! messageLoop()
         | CollectDelegator -> 
           do! collect now
      }
    messageLoop())
  member this.UserCommand cmd = agent.PostAndAsyncReply(fun reply -> UserCommand(cmd, reply))
  member this.WakeUp () = agent.PostAndAsyncReply (fun reply-> WakeUp reply )
  member this.Collect () = agent.Post CollectDelegator
  member this.GetAuctions ()= agent.PostAndAsyncReply(fun reply -> GetAuctions(reply))
  member this.GetAuction auctionId= agent.PostAndAsyncReply(fun reply -> GetAuction(auctionId,reply))

let createAgentDelegator r = AuctionDelegator r