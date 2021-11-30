module Auctions.Program
open System

open Auctions.Web
open Auctions.Actors
open Auctions.Environment
open Auctions.Domain
open Auctions
open FSharpPlus
open FSharpPlus.Data
type DictEntry = System.Collections.DictionaryEntry

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features
open Microsoft.AspNetCore.Authentication
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open System.Text.RegularExpressions
open Giraffe

type CmdArgs =
  { Redis : string option
    Json : string option
    Event : bool
    WebHook : System.Uri option
  }
let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

let configureServices (services : IServiceCollection) =
    services
        .AddGiraffe()
        .AddDataProtection() |> ignore

let configureLogging (loggerBuilder : ILoggingBuilder) =
    loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                 .AddConsole()
                 .AddDebug() |> ignore

[<EntryPoint>]
let main argv =
  // parse arguments
  let defaultArgs =
    { Redis = None
      Json = None
      WebHook = None
      Event = false
    }
  let args =
    let (|Port|_|) : _-> System.UInt16 option = tryParse
    let (|IPAddress|_|) :_->System.Net.IPAddress option = tryParse
    let (|Uri|_|) (uri:string) :System.Uri option = try System.Uri uri |> Some with | _ -> None
    //default bind to 127.0.0.1:8083
    let envArgs = Env.vars () |> Env.envArgs "AUCTIONS_"
    let rec parseArgs b args =
      match args with
      | [] -> b
      | "--redis" :: conn :: xs -> parseArgs { b with Redis = Some conn } xs
      | "--event" :: xs -> parseArgs { b with Event = true } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | "--web-hook" :: Uri url :: xs -> parseArgs { b with WebHook = Some url } xs
      | invalidArgs ->
        printfn "error: invalid arguments %A" invalidArgs
        printfn "Usage:"
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
  let persistEvents = PersistMbox.create batchAppendEvents
  let persistCommands = PersistMbox.create batchAppendCommands
  let observer = Observer.create <| Seq.toList observers
  let time ()= System.DateTime.UtcNow
  let onIncomingCommand command=
    //persist command
    persistCommands command
    Commands [command] |> observer
  let observeCommandResult result =
    iter persistEvents result
    Results [result] |> observer
  // send empty list to observers if any, will cause the program to crash early if observers are misconfigured
  Commands [] |> observer
  let auctionAndStates = if args.Event then Event.foldToMap events else Command.foldToMap commands
                         |> Map.values |> Seq.toList
  let agent = AuctionDelegator.create(auctionAndStates, onIncomingCommand, time, observeCommandResult)
  let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
       .UseGiraffe (Web.webApp agent)

  WebHost.CreateDefaultBuilder()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
  0
