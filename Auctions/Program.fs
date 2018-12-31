module Auctions.Program
open System

open Auctions.Web
open Auctions.Actors
open Auctions
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators
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
    WebHook : Uri option
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
module Env=
  let envArgs (prefix:string) =
    let mangle (str:string) = Regex.Replace(str, "_", "-")
    Seq.cast<DictEntry>( Environment.GetEnvironmentVariables())
    |> Seq.map(fun kv-> (string kv.Key, string kv.Value) )
    |> Seq.filter( fun (key, value) -> key.StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase)
                                       && not <| String.IsNullOrEmpty value)
    |> Seq.collect( fun (key, value) -> [ "--"+ (mangle (key.Substring(0, prefix.Length))).ToLowerInvariant(); value ])
    |> Seq.toList

[<EntryPoint>]
let main argv =
  // parse arguments
  let defaultArgs =
    { Redis = None
      Json = None
      WebHook = None
    }
  let args =
    let (|Port|_|) : _-> UInt16 option = tryParse
    let (|IPAddress|_|) :_->System.Net.IPAddress option = tryParse
    let (|Uri|_|) (uri:string) :System.Uri option = try System.Uri uri |> Some with | _ -> None
    //default bind to 127.0.0.1:8083
    let envArgs = Env.envArgs "AUCTIONS_"
    let rec parseArgs b args =
      match args with
      | [] -> b
      | "--redis" :: conn :: xs -> parseArgs { b with Redis = Some conn } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | "--web-hook" :: Uri url :: xs -> parseArgs { b with WebHook = Some url } xs
      | invalidArgs ->
        printfn "error: invalid arguments %A" invalidArgs
        printfn "Usage:"
        printfn "    --redis                CONN        redis connection string"
        printfn "    --json                 FILE        path to filename to store commands"
        printfn "    --web-hook             URI         web hook to receive commands and command results"
        exit 1

    argv
    |> List.ofArray
    |> List.append envArgs
    |> parseArgs defaultArgs

  let appenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadBatchRedis(args.Redis.Value) :> IAppendBatch
        if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
      }
  let observers = seq {
    if Option.isSome args.WebHook then yield WebHook.ofUri args.WebHook.Value
  }
  let commands = monad.plus {
                  for appender in appenders do
                    yield appender.ReadAll()
                 }
                 |> Async.Parallel |> Async.RunSynchronously
                 |> Seq.collect id
                 |> Seq.toList
  let batchAppend = appenders
                    |> Seq.map (fun a->a.Batch)
                    |> List.ofSeq
  let persist = PersistCommands.create batchAppend
  let observer = Observer.create <| Seq.toList observers
  let time ()= DateTime.UtcNow
  let onIncomingCommand command=
    persist command
    Domain.Commands [command] |> observer
  let observeCommandResult result =
    Domain.Results [result] |> observer
  // send empty list to observers if any, will cause the program to crash early if observers are misconfigured
  Domain.Commands [] |> observer

  let agent = AuctionDelegator.create(commands, onIncomingCommand, time, observeCommandResult)
  let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
       .UseAuthentication()
       .UseGiraffe (Web.webApp agent)

  WebHost.CreateDefaultBuilder()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
  0
