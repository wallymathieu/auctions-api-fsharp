module Auctions.Actors
open Auctions.Domain

open System
open System.Collections.Generic
open FSharpPlus
open Hopac

type AuctionEnded = (Amount * User) option

type AgentSignals = 
  | AgentBid of Bid * IVar<Result<unit, Errors>>
  | AuctionEnded of DateTime

type AuctionAgent(auction, state:S) =
  let inbox = Ch ()
  let validateBid = fun b->Auction.validateBid b auction
  let state = MVar state
  let ended = MVar None

  let agent = Job.foreverServer(job {
      let! msg = Ch.take inbox
      printfn "agent %A is receiving %A" auction.id msg
      match msg with
      | AgentBid(bid, reply) -> 
        match validateBid bid with
        | Ok _ ->
          do! state |> MVar.mutateJob(fun s-> 
            job {
              let (next,res)=S.addBid bid s
              do! IVar.fill reply res 
              return next
            })
        | Error err as self->do! IVar.fill reply self
      | AuctionEnded(now) ->
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
    do! Ch.send inbox (AuctionEnded(time))
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

let createAgent auction bids = AuctionAgent (auction,bids)
type AuctionAndBidsAndMaybeWinnerAndAmount = Auction * (Bid list) * AuctionEnded
type DelegatorSignals = 
  /// From a user command (i.e. create auction or place bid) you expect either a success or an error
  | UserCommand of Command * IVar<Result<CommandSuccess, Errors>>
  | GetAuction of AuctionId * IVar<AuctionAndBidsAndMaybeWinnerAndAmount option>

  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time 
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp
  
//type Agent<'T> = MailboxProcessor<'T>

type AuctionDelegator(commands:Command list, persistCommand, now) = 
  let inbox = Ch ()
  let initialAgents = 
        let _now =now()
        let r = Repository()
        List.iter r.Handle commands
        r.Auctions()
        |> List.map (fun (auction,state)->
                    let next =S.inc _now state
                    auction.id, 
                    if not (S.hasEnded next)
                    then auction,Choice1Of2 (createAgent auction next)
                    else auction,Choice2Of2 (S.tryGetAmountAndWinner next, S.getBids next)
                  )
        |> Map
  let agents : MVar<Map<AuctionId,Auction*Choice<AuctionAgent,AuctionEnded*Bid list>>> = 
        initialAgents
        |> MVar


  /// Note, will mutate agents map
  let ``tryFindTuple_mut`` auctionId : Job<(Auction*Choice<AuctionAgent,AuctionEnded*Bid list>) option> =
    let now = now()
    let res = IVar()
    job {
      do! agents|> MVar.mutateJob(fun a-> job {
            match a |> Map.tryFind auctionId with
            | Some (auction, Choice1Of2 auctionAgent) as v -> 
              let! hasEnded = auctionAgent.AuctionEnded(now)
              match hasEnded with
              | Some ended ->
                let! bids = auctionAgent.GetBids()
                let value =(auction,Choice2Of2 (hasEnded,bids))
                do! IVar.fill res (Some value)
                return (a.Add(auction.id, value))
              | None ->
                do! IVar.fill res v
                return a
            | Some v ->
              do! IVar.fill res (Some v)
              return a
            | None -> 
              do! IVar.fill res None
              return a
          })
      return! IVar.read res 
    }

  /// Note, will mutate agents map
  let ``tryFindAgent_mut`` auctionId :Job<Result<AuctionAgent, Errors>>=
    job{
      let! agent= tryFindTuple_mut auctionId
      match agent with
      | Some (_, Choice1Of2 auctionAgent) ->
        return Ok auctionAgent
      | Some (_, Choice2Of2 _) -> 
        return Error (AuctionHasEnded auctionId)
      | None -> 
        return Error (AuctionNotFound auctionId)
    }

  let userCommand cmd now (reply:IVar<Result<CommandSuccess, Errors>>) : Job<_>=
      job{
        match cmd with
        | AddAuction(at, auction) -> 
          if auction.startsAt > now then
            let agent = createAgent auction (Auction.emptyState auction)
            do! agent.Job
            do! agents|> MVar.mutateFun(fun a-> a.Add(auction.id, (auction ,Choice1Of2 (agent))))
            do! IVar.fill reply (Ok (AuctionAdded(at, auction)))
          else do! IVar.fill reply (Error (AuctionHasEnded auction.id))
        | PlaceBid(at, bid) -> 
          let auctionId = Command.getAuction cmd
          let! maybeAgent = tryFindAgent_mut auctionId
          match maybeAgent with
          | Ok auctionAgent ->
            let! m' = auctionAgent.AgentBid bid 
            do! IVar.fill reply (m' |> Result.map (fun () -> BidAccepted(at, bid)))
          | Error err -> 
            do! IVar.fill reply (Error err)
      }

  let wakeUp now=
    agents |> MVar.mutateJob(fun a->job{
                      let! next = a 
                                  |> Map.toSeq
                                  |> Seq.map (fun (id, (auction, c2) as self)->job{ 
                                    match c2 with
                                    | Choice1Of2 (agent:AuctionAgent)->
                                      let! endedAuction=agent.AuctionEnded(now)
                                      match endedAuction with
                                      | Some _ ->
                                        let! bids= agent.GetBids()
                                        return (id,(auction,Choice2Of2 (endedAuction,bids)))
                                      | None ->
                                        return self
                                    | _ ->
                                      return self
                                  })
                                  |> Job.seqCollect
                      return Map.ofSeq next
    })



  let getAuction auctionId (reply:IVar<AuctionAndBidsAndMaybeWinnerAndAmount option>)= job {
    let! t= tryFindTuple_mut auctionId
    match t with
    | None  -> do! IVar.fill reply None 
    | Some (auction, Choice1Of2 agent) -> 
      let! bids= agent.GetBids()
      do! IVar.fill reply (Some(auction, bids, None))
    | Some (auction, Choice2Of2 (winnerAndAmount,bids)) -> 
      do! IVar.fill reply (Some(auction, bids, winnerAndAmount))
  }
  let agent = Job.foreverServer(job {
    let! msg = Ch.take inbox
    printfn "delegator agent is receiving %A" msg
    let now = now()
    match msg with
      | UserCommand(cmd, reply) -> 
        do persistCommand cmd
        do! userCommand cmd now reply
      | GetAuction (auctionId,reply) ->
        do! getAuction auctionId reply
      | WakeUp -> 
        do! wakeUp now
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
  member __.InitialJobs =
     agent :: (
             initialAgents
             |> Map.toList
             |> List.map (snd>>snd)
             |> List.choose (function 
                  |Choice1Of2 a->Some (a.Job)
                  |Choice2Of2 _ ->None
             ))
             
   

let createAgentDelegator r = AuctionDelegator r
