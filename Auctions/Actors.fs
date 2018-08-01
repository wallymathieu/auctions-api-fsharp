module Auctions.Actors
open Auctions.Domain

open System
open System.Collections.Generic
open FSharpPlus
open Hopac

type AuctionEnded = (Amount * User) option

type AgentSignals = 
  | AgentBid of Bid * IVar<Result<unit, Errors>>
  | HasAuctionEnded of DateTime
with
  override self.ToString() =
    match self with
    | AgentBid (bid,_)-> sprintf "AgentBid (auction: %i, amount: %O, at: %s, user: %O)" bid.auction bid.amount (bid.at.ToString("o")) bid.user
    | HasAuctionEnded (at)-> sprintf "HasAuctionEnded? (at: %s)" (at.ToString("o"))

type AuctionAgent(auction, state:S) =
  let inbox = Ch ()
  let validateBid = fun b->Auction.validateBid b auction
  let state = MVar state
  let ended = MVar None

  let agent = Job.foreverServer(job {
      let! msg = Ch.take inbox
      match msg with
      | AgentBid(bid, reply) -> 
        match validateBid bid with
        | Ok _ ->
          do! state |> MVar.mutateJob(fun s-> 
            job {
              let (next,res)=S.addBid bid s
              do! ended |> MVar.mutateFun (fun _ -> S.tryGetAmountAndWinner next)
              do! IVar.fill reply res 
              return next
            })
        | Error _ as self-> do! IVar.fill reply self
      | HasAuctionEnded now ->
        do! state |> MVar.mutateJob(fun s-> job{
          let next = S.inc now s
          do! ended |> MVar.mutateFun (fun _ -> S.tryGetAmountAndWinner next)
          return next
        })
  })
  
  member __.AgentBid bid :Job<Result<unit,Errors>> = job {
    let reply = IVar()
    do! Ch.send inbox (AgentBid(bid, reply))
    return! IVar.read reply
  }
  member __.GetBids () : Job<Bid list>= job {
    let! s = MVar.read state
    return S.getBids s
  }
  member __.AuctionEnded time : Job<AuctionEnded>= job {
    do! Ch.send inbox (HasAuctionEnded time)
    return! MVar.read ended
  }
  member __.HasEnded time : Job<bool>= job {
    let! ended = __.AuctionEnded time
    return Option.isSome ended
  }
  member __.Job = agent
  

/// repository that takes commands and translate them to auctions and auction states
/// assumption is that it's used in a single threaded sync manner
type private Repository ()= 
  let auctions=Dictionary<AuctionId,Auction*S>()

  member __.Auctions() : (Auction*S) list = 
    auctions.Values |> Seq.toList

  member __.Handle = function 
    | AddAuction (_,a)->
      if not (auctions.ContainsKey a.id) then
        let empty =Auction.emptyState a
        auctions.Add ( a.id, (a,empty) )
      else
        ()
    | PlaceBid (_,b)->
        match auctions.TryGetValue b.auction with
        | true, (auction,state) -> 
          match Auction.validateBid b auction with
          | Ok _ ->
            let (next,_)= S.addBid b state
            auctions.[auction.id]<- (auction,next)
          | Error _ -> ()
        | false, _ -> ()

module AuctionAgent=
  let create auction state = job{
    let a = AuctionAgent (auction,state)
    do! a.Job
    return a
  }
type AuctionAndBidsAndMaybeWinnerAndAmount = Auction * (Bid list) * AuctionEnded
type DelegatorSignals = 
  /// From a user command (i.e. create auction or place bid) you expect either a success or an error
  | UserCommand of Command * IVar<Result<CommandSuccess, Errors>>
  | GetAuction of AuctionId * IVar<AuctionAndBidsAndMaybeWinnerAndAmount option>

  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time 
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp

module AuctionDState=
  type Running=
    | Ongoing of AuctionAgent 
    | Ended of AuctionEnded*Bid list
  type T = Auction * Running
  let (|IsOngoing|HasEnded|) state =
     match state with
     | _, Ongoing agent -> IsOngoing(agent)
     | _, _ -> HasEnded()
  let started auction agent :T= (auction ,Ongoing agent)
  let ended auction endedAuction bids:T=(auction,Ended (endedAuction,bids))

type AuctionDelegator(commands:Command list, persistCommand, now) = 
  let inbox = Ch ()
        
  let agents : MVar<Map<AuctionId,AuctionDState.T>> = MVar()

  let userCommand cmd now (reply:IVar<Result<CommandSuccess, Errors>>) : Job<_>=
      job{
        match cmd with
        | AddAuction(at, auction) -> 
          if auction.startsAt > now then
            let! agent = AuctionAgent.create auction (Auction.emptyState auction)
            do! agents|> MVar.mutateFun(Map.add auction.id (AuctionDState.started auction agent))
            do! IVar.fill reply (Ok (AuctionAdded(at, auction)))
          else do! IVar.fill reply (Error (AuctionHasEnded auction.id))
        | PlaceBid(at, bid) -> 
          let auctionId = Command.getAuction cmd
          let! a= MVar.read agents
          let maybeAgent=match  a |> Map.tryFind auctionId with
                          | Some (AuctionDState.IsOngoing agent) ->
                            Ok agent
                          | Some AuctionDState.HasEnded -> 
                            Error (AuctionHasEnded auctionId)
                          | None -> 
                            Error (AuctionNotFound auctionId)
          match maybeAgent with
          | Ok agent ->
            let! m' = agent.AgentBid bid
            do! IVar.fill reply (m' |> Result.map (fun _ -> BidAccepted(at, bid)))
          | Error err -> 
            do! IVar.fill reply (Error err)
      }

  let wakeUp now=
    agents |> 
      MVar.mutateJob(fun a->job{
        let! next = a 
                    |> Map.toSeq
                    |> Seq.map (fun (id, (auction, c2) as self)->job{ 
                      match c2 with
                      | AuctionDState.Ongoing agent->
                        let! endedAuction=agent.AuctionEnded now
                        match endedAuction with
                        | Some _ ->
                          let! bids= agent.GetBids()
                          return (id,(AuctionDState.ended auction endedAuction bids))
                        | None ->
                          return self
                      | _ ->
                        return self
                    })
                    |> Job.seqCollect
        return Map.ofSeq next
    })

  let getAuction auctionId now (reply:IVar<AuctionAndBidsAndMaybeWinnerAndAmount option>)= job {
    let! a= MVar.read agents
    let state= a |> Map.tryFind auctionId
    match state with
    | None  -> do! IVar.fill reply None 
    | Some (auction, AuctionDState.Ongoing agent) -> 
      let! endedAuction=agent.AuctionEnded now
      let! bids= agent.GetBids()
      do! IVar.fill reply (Some(auction, bids, endedAuction))
    | Some (auction, AuctionDState.Ended (winnerAndAmount, bids) ) -> 
      do! IVar.fill reply (Some(auction, bids, winnerAndAmount))
  }
  let agent = Job.foreverServer(job {
    let! msg = Ch.take inbox
    let _now = now()
    match msg with
      | UserCommand(cmd, reply) -> 
        do persistCommand cmd
        do! userCommand cmd _now reply
      | GetAuction (auctionId,reply) ->
        do! getAuction auctionId _now reply
      | WakeUp -> 
        do! wakeUp _now
  })
  member __.UserCommand cmd =job{
    let reply = IVar()
    do! Ch.send inbox (UserCommand(cmd, reply))
    return! IVar.read reply
  }
  member __.WakeUp () = job{
    do! Ch.send inbox WakeUp
  } 
  member __.GetAuctions ()= job{
    let! a = MVar.read agents 
    return a
           |> Map.toList
           |> List.map (snd>>fst)
  }
  member __.GetAuction auctionId= job{
    let reply = IVar()
    do! Ch.send inbox (GetAuction(auctionId,reply))
    return! IVar.read reply
  }
  member __.Job = 
    let initialAgents = 
      let _now =now()
      let r = Repository()
      List.iter r.Handle commands
      r.Auctions()
      |> List.map (fun (auction,state)->job{
        let next =S.inc _now state
        if not (S.hasEnded next)
        then
          let! agent = AuctionAgent.create auction next
          return auction.id, (AuctionDState.started auction agent)
        else return auction.id, (AuctionDState.ended auction (S.tryGetAmountAndWinner next) (S.getBids next))
      })
    job{
      let! i = Job.seqCollect initialAgents
      do! MVar.fill agents (Map <| List.ofSeq i)
      do! agent
    }
      
module AuctionDelegator=
  let create r = job{
    let d=AuctionDelegator r
    do! d.Job
    return d
  }
