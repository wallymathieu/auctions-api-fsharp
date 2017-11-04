module Auctions.Actors

open System
open Commands
open Domain
open Auction
open System.Collections.Generic

type Agent<'T> = MailboxProcessor<'T>


type AgentSignals = 
  | AgentBid of Bid * AsyncReplyChannel<Result<unit, Errors>>
  | GetBids of AsyncReplyChannel<Bid list>
  | AuctionEnded of DateTime * AsyncReplyChannel<AuctionEnded>
  | CollectAgent of DateTime * AsyncReplyChannel<AuctionEnded>

type AuctionAgent(auction, bids) =
  let agent = Agent<AgentSignals>.Start(fun inbox -> 
    (let validateBid = validateBid auction
     let validateBidForAuctionType = validateBidForAuctionType auction 
     let mutable bids = bids
     
     /// try to signal auction ended
     let tryGetAuctionEnded time : AuctionEnded = 
       Auction.getAmountAndWinner auction bids time
     
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
type AuctionAndBids = Auction * (Choice<Bid list,(Amount*User) option>)
type DelegatorSignals = 
  /// From a user command (i.e. create auction or place bid) you expect either a success or an error
  | UserCommand of Command * AsyncReplyChannel<Result<CommandSuccess, Errors>>
  | GetAuction of AuctionId *AsyncReplyChannel<AuctionAndBids option>
  | GetAuctions of AsyncReplyChannel<Auction list>

  /// ping delegator to make sure that time based logic can run (i.e. CRON dependent auction logic)
  /// this is needed due to the fact that you have auction end time 
  /// (you expect something to happen roughly then, and not a few hours later)
  | WakeUp of AsyncReplyChannel<unit>
  /// Do we need to do anything if we have a handled shutdown of the delegator?
  | CollectDelegator
type AuctionDelegator(r, persistCommand, now) = 
  let agent =Agent<DelegatorSignals>.Start(fun inbox -> 
    let mutable agents = let _now =now()
                         r |> Repo.auctions
                         |> List.map (fun auction->
                                        auction.id, 
                                        if (not<< Auction.hasEnded _now) auction
                                        then auction ,Choice1Of2 (createAgent auction (r.GetBidsForAuction auction.id))
                                        else auction ,Choice2Of2 (Auction.getAmountAndWinner auction (r.GetBidsForAuction auction.id) _now)
                                      )
                         |> Map

    /// Note, will mutate agents map
    let tryFindTuple_mut auctionId=
      let now = now()
      let auctionHasEnded = Auction.hasEnded now
      async{
      match agents |> Map.tryFind auctionId with
      | Some (auction, Choice1Of2 auctionAgent) when auctionHasEnded auction -> 
        let! ended= auctionAgent.Collect now
        let value =(auction,Choice2Of2 ended)
        agents <- agents.Add(auction.id, value)
        return Some value
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
        return Error (AuctionHasEnded auction.id)
      | None -> 
        return Error (AuctionNotFound auctionId)
      }

    let userCommand cmd now (reply:AsyncReplyChannel<Result<CommandSuccess, Errors>>)=
       async{
         let auctionHasEnded = Auction.hasEnded now
         match cmd with
         | AddAuction(at, auction) -> 
           if not (auctionHasEnded auction) then
             agents <- agents.Add(auction.id, (auction ,Choice1Of2 (createAgent auction [])))
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
       let auctionHasEnded = Auction.hasEnded now
       let hasEnded = agents 
                          |> Map.toSeq
                          |> Seq.map snd
                          |> Seq.filter (fun (a,c)-> auctionHasEnded a 
                                                     && match c with | Choice1Of2 agent->true | _->false)
       async {
         for (auction,c) in hasEnded do 
            do! c |> function 
                  | Choice1Of2 agent -> 
                      async{
                        let! endedAuction = agent.Collect now
                        do agents <- agents.Add (auction.id,(auction,Choice2Of2 endedAuction))
                        // NOTE: here we might want to send out a signal
                        return ()
                      }
                  | Choice2Of2 _ -> async { return () }
         return ()
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

    let getAuction auctionId (reply:AsyncReplyChannel<AuctionAndBids option>)=
      async {
      let! t= tryFindTuple_mut auctionId
      match t with
      | None  ->  reply.Reply None 
      | Some (auction, Choice1Of2 agent) -> 
        let! bids= agent.GetBids()
        reply.Reply (Some(auction, Choice1Of2 bids))
      | Some (auction, Choice2Of2 ended) -> 
        reply.Reply (Some(auction, Choice2Of2 ended)) 
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
            //reply.Reply( auctions |> List.tryFind (fun a->a.id=auctionId) )
         | GetAuctions (reply) ->
            let auctions =agents 
                          |> Map.toList
                          |> List.map (snd >> fst)
            reply.Reply auctions
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