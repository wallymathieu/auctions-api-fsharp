namespace Auctions
open Auctions.Domain
open System

type PersistCommands(appendBatches : (Command list -> Async<unit>) list) = 
  let mbox = MailboxProcessor.Start(fun inbox ->
       let rec messageLoop() = 
         async {
           let! command = inbox.Receive()
           let toAppend = [command]
           for appendBatch in appendBatches do 
             do! appendBatch toAppend
           return! messageLoop()
         }
       messageLoop())
  
  member __.Handle(command) = 
    mbox.Post(command)
