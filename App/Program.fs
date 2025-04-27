module Auctions.Program
open System
open Suave
open Auctions.Web
open Auctions.Actors
open Auctions.Environment
open Auctions.Domain
open Auctions
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators
open Hopac
type DictEntry = System.Collections.DictionaryEntry

type CmdArgs =
  { IP : System.Net.IPAddress
    Port : Sockets.Port
    Redis : string option
    Json : string option
    Event : bool
    WebHook : System.Uri option
  }

[<EntryPoint>]
let main argv =
  // parse arguments
  let args =
    let (|Port|_|) : _-> System.UInt16 option = tryParse
    let (|IPAddress|_|) :_->System.Net.IPAddress option = tryParse
    let (|Uri|_|) (uri:string) :System.Uri option = try System.Uri uri |> Some with | _ -> None
    //default bind to 127.0.0.1:8083
    let defaultArgs =
      { IP = System.Net.IPAddress.Loopback
        Port = 8083us
        Redis = None
        Json = None
        WebHook = None
        Event = false
      }
    let envArgs = Env.vars () |> Env.envArgs "AUCTIONS_"
    let rec parseArgs b args =
      match args with
      | [] -> b
      | "--ip" :: IPAddress ip :: xs -> parseArgs { b with IP = ip } xs
      | "--port" :: Port p :: xs -> parseArgs { b with Port = p } xs
      | "--redis" :: conn :: xs -> parseArgs { b with Redis = Some conn } xs
      | "--event" :: xs -> parseArgs { b with Event = true } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | "--web-hook" :: Uri url :: xs -> parseArgs { b with WebHook = Some url } xs
      | invalidArgs ->
        printfn $"error: invalid arguments %A{invalidArgs}"
        printfn "Usage:"
        printfn $"    --ip                   ADDRESS     ip address (Default: {defaultArgs.IP})"
        printfn $"    --port                 PORT        port (Default: %i{defaultArgs.Port})"
        printfn "    --redis                CONN        redis connection string"
        printfn "    --json                 FILE        path to filename to store commands"
        printfn "    --event                            if you want to replay events instead of commands"
        printfn "    --web-hook             URI         web hook to receive commands and command results"
        exit 1

    argv
    |> List.ofArray
    |> List.append envArgs
    |> parseArgs defaultArgs

  let eventAppenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadEventBatchRedis(args.Redis.Value) :> IAppendBatch<Event>
        if Option.isSome args.Json && args.Event then yield JsonAppendEventToFile(args.Json.Value) :> IAppendBatch<Event>
      }
  let commandAppenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadCommandBatchRedis(args.Redis.Value) :> IAppendBatch<Command>
        if Option.isSome args.Json then yield JsonAppendCommandToFile(args.Json.Value) :> IAppendBatch<Command>
      }
  let observers = seq {
    if Option.isSome args.WebHook then yield WebHook.ofUri args.WebHook.Value
  }
  let collectAsync seq =
                 seq
                 |> Async.Parallel |> Async.RunSynchronously
                 |> Seq.collect id
                 |> Seq.toList
  let events =   seq { for appender in eventAppenders do yield appender.ReadAll() }
                 |> collectAsync

  let commands = seq { for appender in commandAppenders do yield appender.ReadAll() }
                 |> collectAsync

  let batchAppendEvents = eventAppenders |> Seq.map (fun a->a.Batch) |> List.ofSeq
  let batchAppendCommands = commandAppenders |> Seq.map (fun a->a.Batch) |> List.ofSeq
  let observer = Observer.create <| Seq.toList observers
  let time ()= DateTime.UtcNow
  let agent= job{
    let! commandPersister = Persister.create batchAppendCommands
    let! eventPersister = Persister.create batchAppendEvents
    let! observer = Observer.create <| Seq.toList observers
    let onIncomingCommand command=job {
      do! command |> commandPersister
      do! Domain.Commands [command] |> observer
    }
    let observeCommandResult result = job {
      match result with
      | Ok v -> do! eventPersister v
      | _ -> ()
      do! Domain.Results [result] |> observer
    }
    // send empty list to observers if any, will cause the program to crash early if observers are misconfigured
    do! Domain.Commands [] |> observer

    return! AuctionDelegator.create(commands, onIncomingCommand, time, observeCommandResult)
  }

  let auctionAndStates = if args.Event then Event.foldToMap events else Command.foldToMap commands
                         |> Map.values |> Seq.toList
  // start suave
  startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP args.IP args.Port ] } (OptionT.run << webPart (run agent) time)
  0
