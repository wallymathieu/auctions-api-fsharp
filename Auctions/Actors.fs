module Auctions.Actors

open System.Threading
open System.Threading.Tasks
open Commands
open Domain
open Listeners
(*
Calculate running auctions : init agents

signal -> delegator (calculate x) -> agents.[x] 

signal start auction -> 
                        agent.Start()
                        agents <- agent :: agents 
singal end auction ->
                        agent.Stop()
                        agents <- agents |> List.except [agent]


command    -> agents.[x] --> Maybe error  -> auction listeners
           \             \
            \             \-> Auction Update -> auction listeners
             |                               \
             |                                \-> send state to read model
             |
             \- persisters.[...] -> persist command


auction listener : only interested in changes that it does not know of, i.e.
                   when it's started, it takes a param that tells 



Assumptions:

- each auction get 1 agent 
- all commands are logged in redis, json et.c. 
- one dedicated thread per command persister (json, redis)


query      -> query agent.[y] --> Result<QueryResult,QueryError>

*)

type Agent<'T> = MailboxProcessor<'T>

//let createDispatcher 

let createAgent 
    (errorQueue : MailboxProcessor<Error>) 
    (listener : MailboxProcessor<CommandSuccess>)
        = 
  Agent<Command* AsyncReplyChannel<Error option>>.Start(fun inbox -> 
    (let r = ConcurrentRepository()
     
     let rec messageLoop() = 
       async { 
         let! (msg,reply) = inbox.Receive()
         let maybeError = handleCommand r msg
         match maybeError with
         | Ok success -> 
             listener.Post success
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
         printfn "%A" msg // send signal using websocket
         return! messageLoop()
       }
     messageLoop()))

let createListener r = 
  Agent<CommandSuccess>.Start(fun inbox -> 
    (let rec messageLoop() = 
       async { 
         let! msg = inbox.Receive()
         match listenTo r msg with
         | Ok _ -> () // send signal using websocket
         | Error err -> failwithf "%A" err // should not throw an error, should notify dispatcher 
         return! messageLoop()
       }
     messageLoop()))
