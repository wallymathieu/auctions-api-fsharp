module Auctions.Program
open Suave
open System

open Auctions.Web
open Auctions.Actors
open Auctions
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators
type DictEntry = System.Collections.DictionaryEntry

type CmdArgs =
  { IP : System.Net.IPAddress
    Port : Sockets.Port
    Redis : string option
    Json : string option
    CommandsWebHook : Uri option
  }
module Env=
  let envArgs (prefix:string) =
    let mangle (str:string) = Regex.replace "_" "-" str
    Seq.cast<DictEntry>( Environment.GetEnvironmentVariables())
    |> Seq.map(fun kv-> (string kv.Key, string kv.Value) )
    |> Seq.filter( fun (key, value) -> key.StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase)
                                       && not <| String.IsNullOrEmpty value)
    |> Seq.collect( fun (key, value) -> [ "--"+ (mangle (key.Substring(0, prefix.Length))).ToLowerInvariant(); value ])
    |> Seq.toList

[<EntryPoint>]
let main argv =
  // parse arguments
  let args =
    let (|Port|_|) : _-> UInt16 option = tryParse
    let (|IPAddress|_|) :_->System.Net.IPAddress option = tryParse
    let (|Uri|_|) (uri:string) :System.Uri option = try System.Uri uri |> Some with | _ -> None
    //default bind to 127.0.0.1:8083
    let defaultArgs =
      { IP = System.Net.IPAddress.Loopback
        Port = 8083us
        Redis = None
        Json = None
        CommandsWebHook = None
      }
    let envArgs = Env.envArgs "AUCTIONS_"
    let rec parseArgs b args =
      match args with
      | [] -> b
      | "--ip" :: IPAddress ip :: xs -> parseArgs { b with IP = ip } xs
      | "--port" :: Port p :: xs -> parseArgs { b with Port = p } xs
      | "--redis" :: conn :: xs -> parseArgs { b with Redis = Some conn } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | "--commands-web-hook" :: Uri url :: xs -> parseArgs { b with CommandsWebHook = Some url } xs
      | invalidArgs ->
        printfn "error: invalid arguments %A" invalidArgs
        printfn "Usage:"
        printfn "    --ip                   ADDRESS     ip address (Default: %O)" defaultArgs.IP
        printfn "    --port                 PORT        port (Default: %i)" defaultArgs.Port
        printfn "    --redis                CONN        redis connection string"
        printfn "    --json                 FILE        path to filename to store commands"
        printfn "    --commands-web-hook    URI         web hook to receive commands"
        exit 1

    argv
    |> List.ofArray
    |> List.append envArgs
    |> parseArgs defaultArgs

  let appenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadBatchRedis(args.Redis.Value) :> IAppendBatch
        if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
      }
  let appendOnly = seq {
    if Option.isSome args.CommandsWebHook then yield WebHook.commands args.CommandsWebHook.Value Console.Error.WriteLine
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
                    |> Seq.append appendOnly
                    |> List.ofSeq
  let persist = PersistCommands batchAppend

  let agent = AuctionDelegator.create(commands, persist.Handle, fun ()->DateTime.UtcNow)
  // start suave
  startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP args.IP args.Port ] } (OptionT.run << webPart agent)
  0
