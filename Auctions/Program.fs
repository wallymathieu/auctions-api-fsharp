module App
open Giraffe
open System

open Auctions.Web
open Auctions.Actors
open Auctions

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features
open Microsoft.AspNetCore.Authentication
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

type CmdArgs = 
  { Redis : string option
    Json : string option
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
  let args = 
    let defaultArgs = 
      { Redis = None
        Json = None
      }
    
    let rec parseArgs b args = 
      match args with
      | [] -> b
      | "--redis" :: conn :: xs -> parseArgs { b with Redis = Some conn } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | invalidArgs -> 
        printfn "error: invalid arguments %A" invalidArgs
        printfn "Usage:"
        printfn "    --redis connection"
        printfn "    --json file"
        exit 1
    
    argv
    |> List.ofArray
    |> parseArgs defaultArgs
  
  let appenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadBatchRedis(args.Redis.Value) :> IAppendBatch
        if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
      }
  let persist = PersistCommands (appenders |> Seq.map (fun a->a.Batch) |> List.ofSeq)
  let commands = appenders
                 |> Seq.collect (fun a->a.ReadAll()) 
                 |> Seq.toList

  persist.Start()

  
  let agent = createAgentDelegator(commands, persist.Handle, fun ()->DateTime.UtcNow)
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
