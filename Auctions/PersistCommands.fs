namespace Auctions
open Auctions.Domain

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading

type PersistCommands(appendBatches : (Command list -> unit) list) = 
  let notNull = not << isNull
  let mutable thread = null
  let stop = ref false
  let commands = new ConcurrentQueue<Command>()
  let signal = new EventWaitHandle(false, EventResetMode.AutoReset)
  
  let appendBatch() = 
    let receivedCommands = new List<Command>(10)
    let command =ref (Unchecked.defaultof<Command>)
    while (commands.TryDequeue(command)) do
      receivedCommands.Add(!command)
    let toAppend=receivedCommands |> Seq.toList
    for appendBatch in appendBatches do 
      appendBatch toAppend
  
  member __.ThreadStart() = 
    while (not stop.Value) do
      signal.WaitOne() |> ignore
      appendBatch()
    // While the batch has been running, more commands might have been added
    // and stop might have been called
    appendBatch()
  
  member this.Start() = 
    if (notNull thread) then failwith ("already started")
    else 
      thread <- Thread(this.ThreadStart)
      thread.Start()
  
  member __.Started() = notNull thread
  
  member __.Stop() = 
    stop := true
    signal.Set() |> ignore
    if (notNull thread) then thread.Join()
    else ()
  
  member __.Handle(command) = 
    // send the command to separate thread and persist it
    commands.Enqueue(command)
    signal.Set() |> ignore
