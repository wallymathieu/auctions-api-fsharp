module Auctions.Actors

open System.Threading
open System.Threading.Tasks
open Commands
open Domain

(*
Calculate running auctions : init agents

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

*)

//let createDispatcher 
type Agent<'T> = MailboxProcessor<'T>

type AuctionAgent = Agent<Command* AsyncReplyChannel<Result<CommandSuccess,Errors>>>
let createAgent 
        = 
  AuctionAgent.Start(fun inbox -> 
    (let r = ConcurrentRepository()
     
     let rec messageLoop() = 
       async { 
         let! (msg,reply) = inbox.Receive()
         reply.Reply (handleCommand r msg |> Result.map snd)
         return! messageLoop()
       }
     messageLoop()))
