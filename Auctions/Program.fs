open Suave
open System

open Auctions.Web
open Auctions.Actors
open Auctions
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators
type CmdArgs = 
  { IP : System.Net.IPAddress
    Port : Sockets.Port
    Redis : string option
    Json : string option
  }
[<EntryPoint>]
let main argv = 
  // parse arguments
  let args = 
    let (|Port|_|) : _-> UInt16 option = tryParse
    let (|IPAddress|_|) :_->System.Net.IPAddress option= tryParse
    
    //default bind to 127.0.0.1:8083
    let defaultArgs = 
      { IP = System.Net.IPAddress.Loopback
        Port = 8083us
        Redis = None
        Json = None
      }
    
    let rec parseArgs b args = 
      match args with
      | [] -> b
      | "--ip" :: IPAddress ip :: xs -> parseArgs { b with IP = ip } xs
      | "--port" :: Port p :: xs -> parseArgs { b with Port = p } xs
      | "--redis" :: conn :: xs -> parseArgs { b with Redis = Some conn } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | invalidArgs -> 
        printfn "error: invalid arguments %A" invalidArgs
        printfn "Usage:"
        printfn "    --ip ADDRESS   ip address (Default: %O)" defaultArgs.IP
        printfn "    --port PORT    port (Default: %i)" defaultArgs.Port
        exit 1
    
    argv
    |> List.ofArray
    |> parseArgs defaultArgs
  
  let appenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadBatchRedis(args.Redis.Value) :> IAppendBatch
        if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
      }
  let persist = PersistCommands (appenders |> Seq.map (fun a->a.Batch) |> List.ofSeq)
  let commands = monad.plus {
                  for appender in appenders do
                    yield appender.ReadAll()
                 }
                 |> Async.Parallel |> Async.RunSynchronously
                 |> Seq.collect id
                 |> Seq.toList
  let agent = AuctionDelegator.create(commands, persist.Handle, fun ()->DateTime.UtcNow)
  // start suave
  startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP args.IP args.Port ] } (OptionT.run << webPart agent)
  0
