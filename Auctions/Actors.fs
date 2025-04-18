module Auctions.Actors
open Auctions.Domain

open System
open FSharpPlus

let exitOnException (e:exn)=
  printfn $"Failed with exception %s{e.Message}, %s{e.StackTrace}, exit!"
  exit 1

module PersistMbox=
  /// Create persist messagebox
  let create(appendBatches : ('T list -> Async<unit>) list) =
    let mbox = MailboxProcessor.Start(fun inbox ->
       let rec messageLoop() =
         async {
           let! event = inbox.Receive()
           let toAppend = [event]
           for appendBatch in appendBatches do
             do! appendBatch toAppend
           return! messageLoop()}
       messageLoop())

    mbox.Error.Add exitOnException

    mbox.Post

module Observer=
  let create<'t>(observers : ('t-> Async<unit>) list) =
    let mbox = MailboxProcessor.Start(fun inbox ->
      let rec messageLoop() =
        async {
          let! input = inbox.Receive()
          for observe in observers do
              do! observe input
          return! messageLoop()}
      messageLoop())

    mbox.Error.Add exitOnException

    mbox.Post

type AuctionEnded = (Amount * User) option

type AgentSignals =
  | AgentBid of Bid * AsyncReplyChannel<Result<unit, Errors>>
  | GetBids of AsyncReplyChannel<Bid list>
  | HasAuctionEnded of DateTime * AsyncReplyChannel<AuctionEnded>
  | CollectAgent of DateTime * AsyncReplyChannel<AuctionEnded>

type AuctionAgent(auction, state:S) =
  let agent = MailboxProcessor<AgentSignals>.Start(fun inbox ->
    (let validateBid = Auction.validateBid auction
     let mutable state = state

     let rec messageLoop() =
       async {
         let! msg = inbox.Receive()
         match msg with
         | AgentBid(bid, reply) ->
           reply.Reply(monad {
                         do! validateBid bid
                         let next,res = S.addBid bid state
                         state <- next
                         return! res
                       })
           return! messageLoop()
         | GetBids reply->
           let bids = S.getBids state
           reply.Reply bids
           return! messageLoop()
         | HasAuctionEnded(now, reply) ->
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
       }

     messageLoop()))
  do
    agent.Error.Add exitOnException
  member _.AgentBid bid = agent.PostAndAsyncReply(fun reply -> AgentBid(bid, reply))
  member _.GetBids () = agent.PostAndAsyncReply(fun reply -> GetBids(reply))
  member _.AuctionEnded time = agent.PostAndAsyncReply(fun reply -> HasAuctionEnded(time, reply))
  member _.Collect time = agent.PostAndAsyncReply(fun reply -> CollectAgent(time, reply))
  member self.HasEnded time = async{
    let! res=self.AuctionEnded time
    return Option.isSome res
  }

module AuctionAgent=
  let create auction state = AuctionAgent (auction, state)

type AuctionAndBidsAndMaybeWinnerAndAmount = Auction * Bid list * AuctionEnded
type DelegatorSignals =
  /// From a user command (i.e. create auction or place bid) you expect either a success or an error
  | UserCommand of Command * AsyncReplyChannel<Result<Event, Errors>>
  | GetAuction of AuctionId *AsyncReplyChannel<AuctionAndBidsAndMaybeWinnerAndAmount option>
  | GetAuctions of AsyncReplyChannel<Auction list>

  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp of AsyncReplyChannel<unit>
module AuctionDState=
  type Running=
    | Ongoing of AuctionAgent
    | Ended of AuctionEnded * Bid list
  type T = Auction * Running
  let (|IsOngoing|HasEnded|) state =
     match state with
     | _, Ongoing agent -> IsOngoing(agent)
     | _, _ -> HasEnded()
  let started auction agent :T= (auction ,Ongoing agent)
  let ended auction endedAuction bids:T=(auction,Ended (endedAuction,bids))
type AuctionDelegator(auctions: (Auction*S) list, onIncomingCommand, now, observeResult) =
  let agent =MailboxProcessor<DelegatorSignals>.Start(fun inbox ->
    let mutable agents = let _now =now()
                         auctions
                         |> List.map (fun (auction,state)->
                                        let next =S.inc _now state
                                        auction.id,
                                        if not (S.hasEnded next)
                                        then AuctionDState.started auction (AuctionAgent.create auction next)
                                        else AuctionDState.ended auction (S.tryGetAmountAndWinner next) (S.getBids next)
                                      )
                         |> Map

    let userCommand cmd (reply:AsyncReplyChannel<Result<Event, Errors>>)=
      let observeAndReply result=
         observeResult result
         reply.Reply result
      async{
         match cmd with
         | AddAuction(at, auction) ->
            match (auction.expiry > at, Map.containsKey auction.id agents) with
            | true, false ->
              let agent = AuctionAgent.create auction (Auction.emptyState auction)
              agents <- Map.add auction.id (AuctionDState.started auction agent) agents
              observeAndReply (Ok (AuctionAdded(at, auction)))
            | false, _ ->
              observeAndReply (Error (AuctionHasEnded auction.id))
            | _, true ->
              observeAndReply (Error (AuctionAlreadyExists auction.id))
         | PlaceBid(at, bid) ->
           let auctionId = Command.getAuction cmd
           let maybeAgent=match Map.tryFind auctionId agents with
                          | Some (AuctionDState.IsOngoing agent) ->
                            Ok agent
                          | Some AuctionDState.HasEnded ->
                            Error (AuctionHasEnded auctionId)
                          | None ->
                            Error (AuctionNotFound auctionId)
           match maybeAgent with
           | Ok auctionAgent ->
              let! m = auctionAgent.AgentBid bid
              let result = m |> Result.map (fun () -> BidAccepted(at, bid))
              observeAndReply result
           | Error err ->
              observeAndReply (Error err)
       }

    let wakeUp now=
      async {
        let! next =agents
                    |> Map.toSeq
                    |> Seq.map (fun (id, (auction, r) as self)->async{
                      match r with
                      | AuctionDState.Ongoing agent->
                        let! endedAuction=agent.AuctionEnded now
                        match endedAuction with
                        | Some _ ->
                          let! bids= agent.GetBids()
                          do! Async.Ignore (agent.Collect now)
                          return (id,(AuctionDState.ended auction endedAuction bids))
                        | None ->
                          return self
                      | _ ->
                        return self
                    })
                    |> Async.Parallel
        agents<-Map next
      }

    let getAuction auctionId now (reply:AsyncReplyChannel<AuctionAndBidsAndMaybeWinnerAndAmount option>)=
      async {
      match Map.tryFind auctionId agents with
      | None  ->  reply.Reply None
      | Some (auction, AuctionDState.Ongoing agent) ->
        let! endedAuction=agent.AuctionEnded now
        let! bids= agent.GetBids()
        reply.Reply (Some(auction, bids, endedAuction))
      | Some (auction, AuctionDState.Ended (winnerAndAmount,bids)) ->
        reply.Reply (Some(auction, bids, winnerAndAmount))
      }
    let rec messageLoop() =
      async {
        let! msg = inbox.Receive()
        let now = now()
        match msg with
         | UserCommand(cmd, reply) ->
           do onIncomingCommand cmd
           do! userCommand cmd reply
           return! messageLoop()
         | GetAuction (auctionId,reply) ->
           do! getAuction auctionId now reply
           return! messageLoop()
         | GetAuctions reply ->
           let auctions =agents
                          |> Map.toList
                          |> List.map (snd>>fst)
           reply.Reply auctions
           return! messageLoop()
         | WakeUp reply ->
           do! wakeUp now
           do reply.Reply()
           return! messageLoop()
      }
    messageLoop())
  do
    agent.Error.Add exitOnException
  member _.UserCommand cmd = agent.PostAndAsyncReply(fun reply -> UserCommand(cmd, reply))
  member _.WakeUp () = agent.PostAndAsyncReply WakeUp
  member _.GetAuctions ()= agent.PostAndAsyncReply(GetAuctions)
  member _.GetAuction auctionId= agent.PostAndAsyncReply(fun reply -> GetAuction(auctionId,reply))
module AuctionDelegator=
  let create r = AuctionDelegator r
