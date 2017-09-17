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


command    -> agents.[x] --> Maybe error
           \             \
            \             \-> post auction update to clients
             |            |
             |            \-> send state to read model
             |
             \- persisters.[...] -> persist command




Assumptions:

- each auction get 1 agent 
- all commands are logged in redis, json et.c. 
- one dedicated thread per command persister (json, redis)


query      -> agents.[x] --> Result<QueryResult,QueryError>

*)

type Agent<'T> = MailboxProcessor<'T>

let createAgent (errorQueue : MailboxProcessor<Error>) = 
  Agent<Command* AsyncReplyChannel<Error option>>.Start(fun inbox -> 
    (let r = ConcurrentRepository()
     
     let rec messageLoop() = 
       async { 
         let! (msg,reply) = inbox.Receive()
         let maybeError = handleCommand r msg
         match maybeError with
         | Ok() -> ()
         | Error e -> 
             reply.Reply (Some e)
             errorQueue.Post e

         return! messageLoop()
       }
     messageLoop()))

let createErrorListener() = 
  Agent<Error>.Start(fun inbox -> 
    (let rec messageLoop() = 
       async { 
         let! msg = inbox.Receive()
         printfn "%A" msg
         return! messageLoop()
       }
     messageLoop()))


