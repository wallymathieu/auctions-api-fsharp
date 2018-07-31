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
  //| HasEnded of DateTime * AsyncReplyChannel<Boolean>
  //| CollectAgent of DateTime * AsyncReplyChannel<AuctionEnded>

type AuctionAgent(auction, state:S) =
  let inbox = Ch ()
  let validateBid = fun b->Auction.validateBid b auction
  let state = MVar state
  let ended = MVar None

  let agent = Job.foreverServer(monad { 
      let! msg = Ch.take inbox
      match msg with
      | AgentBid(bid, reply) -> 
        do! validateBid bid
        do! state |> MVar.mutateJob(fun s-> 
          monad {
            let (next,res)=S.addBid bid s
            do! IVar.fill reply res 
            return next
          })
      | AuctionEnded(now) ->
        do! state |> MVar.mutateJob(fun s-> monad{
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
  
  //member __.Collect time = agent.PostAndAsyncReply(fun reply -> CollectAgent(time, reply))
  //member __.HasEnded time = agent.PostAndAsyncReply(fun reply -> AgentSignals.HasEnded(time, reply))

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
  | UserCommand of Command * AsyncReplyChannel<Result<CommandSuccess, Errors>>
  | GetAuction of AuctionId *AsyncReplyChannel<AuctionAndBidsAndMaybeWinnerAndAmount option>
  | GetAuctions of AsyncReplyChannel<Auction list>

  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time 
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp of AsyncReplyChannel<unit>
  /// Do we need to do anything if we have a handled shutdown of the delegator?
  | CollectDelegator
  
//type Agent<'T> = MailboxProcessor<'T>

type AuctionDelegator(commands:Command list, persistCommand, now) = 
  let inbox = Ch ()
  let agents : MVar<Map<AuctionId,Auction*Choice<AuctionAgent,AuctionEnded*Bid list>>> = 
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
        |> MVar


  /// Note, will mutate agents map
  let ``tryFindTuple_mut`` auctionId : Job<(Auction*Choice<AuctionAgent,AuctionEnded*Bid list>) option> =
    let now = now()
    let res = IVar()
    job {
      do! agents|> MVar.mutateJob(fun a-> monad {
            match a |> Map.tryFind auctionId with
            | Some (auction, Choice1Of2 auctionAgent) as v -> 
              let! hasEnded = auctionAgent.AuctionEnded(now)
              match hasEnded with
              | Some ended ->
                let! bids = auctionAgent.GetBids()
                let value =(auction,Choice2Of2 (ended,bids))
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
            do! agents|> MVar.mutateFun(fun a-> a.Add(auction.id, (auction ,Choice1Of2 (createAgent auction (Auction.emptyState auction)))))
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
    let hasEnded id=function
                  | Choice1Of2 (agent:AuctionAgent)-> 
                    job{
                      let! ended=agent.HasEnded(now)
                      return if ended then Some (id,agent) else None
                    }
                  | _-> 
                    job { return None }
    let hasEnded = agents 
                        |> Map.toSeq
                        |> Seq.map (fun (id, (_, c))-> hasEnded id c)
                        |> Job.seqCollect


    let endAgent id (agent:AuctionAgent)= 
        monad{
          let (auction,_) = Map.find id agents
          let! bids= agent.GetBids()
          let! endedAuction = agent.Collect now
          do agents <- agents.Add (auction.id,(auction,Choice2Of2 (endedAuction,bids)))
        }
    monad {
      let! endedAgents =hasEnded
      for (id,agent) in endedAgents |> Array.choose id  do 
          do! endAgent id agent
    }



  let getAuction auctionId (reply:AsyncReplyChannel<AuctionAndBidsAndMaybeWinnerAndAmount option>)= job {
    let! t= tryFindTuple_mut auctionId
    match t with
    | None  ->  reply.Reply None 
    | Some (auction, Choice1Of2 agent) -> 
      let! bids= agent.GetBids()
      reply.Reply (Some(auction, bids, None))
    | Some (auction, Choice2Of2 (winnerAndAmount,bids)) -> 
      reply.Reply (Some(auction, bids, winnerAndAmount))
  }
  let agent = Job.foreverServer(monad {
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
  })
  member __.UserCommand cmd = agent.PostAndAsyncReply(fun reply -> UserCommand(cmd, reply))
  member __.WakeUp () = agent.PostAndAsyncReply WakeUp
  member __.Collect () = agent.Post CollectDelegator
  member __.GetAuctions ()= agent.PostAndAsyncReply(fun reply -> GetAuctions(reply))
  member __.GetAuction auctionId= agent.PostAndAsyncReply(fun reply -> GetAuction(auctionId,reply))

let createAgentDelegator r = AuctionDelegator r
