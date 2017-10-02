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

type AgentSignals = 
  | Bid of Bid
  | AuctionEnded
  | CollectAgent

type AuctionAgent = Agent<AgentSignals * AsyncReplyChannel<Result<Bid, Errors>>>

let createAgent auction auctionEnded = 
  AuctionAgent.Start(fun inbox -> 
    (let validateBid = validateBid auction
     let mutable bids = []
     
     let maxBid() = 
       if List.isEmpty bids then None
       else Some(bids |> List.maxBy (fun b -> b.amount)) // we assume that we use a fixed currency
     
     let rec messageLoop() = 
       async { 
         let! (msg, reply) = inbox.Receive()
         match msg with
         | Bid bid -> 
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
                         do! if bid.amount.currency <> Currency.VAC then 
                               Error(Errors.BidCurrencyConversion(bid.id, bid.amount.currency)) 
                             else Ok()
                         bids <- bid :: bids
                         return bid
                       })
           return! messageLoop()
         | AuctionEnded -> 
           (*
            When the agent receives this signal 
            - it can start replying with only bid rejected response
            - make sure to send out signal about auction status (if there is a winner)
            *)
           let max = maxBid()
           auctionEnded (auction,max)
           return! messageLoop()
         | CollectAgent -> 
           (*
            When the agent receives this signal
            - it should collect any dangling business rules
                - for instance send out auction end signals 
            - quit
            *)
           let max = maxBid()
           auctionEnded (auction,max)
           return ()
       }
     
     messageLoop()))

type AuctionDelegator = Agent<Command * AsyncReplyChannel<Result<CommandSuccess, Errors>>>

let createDelegator r = 
  AuctionDelegator.Start(fun inbox -> 
    (let mutable auctions = []
     
     let rec messageLoop() = 
       async { 
         let! (msg, reply) = inbox.Receive()
         let now = DateTime.UtcNow
         (* 
          
         *)
         return! messageLoop()
       }
     messageLoop()))