namespace Auctions
open Auctions.Domain

open System.IO
open System
open Fleece
open Fleece.FSharpData
open FSharp.Data

type JsonAppendToFile(fileName) =
  let notNull= not << isNull
  let fileDoesNotExist = not << File.Exists
  do
    if fileDoesNotExist fileName then File.WriteAllText(fileName, "")

  interface IAppendBatch with
    member __.Batch cs = async{
      use fs = File.Open(fileName, FileMode.Append, FileAccess.Write, FileShare.Read)
      use w = new StreamWriter(fs)
      let json = toJson cs
      json.WriteTo (w, JsonSaveOptions.DisableFormatting)
      do! w.WriteLineAsync()
      do! fs.FlushAsync()
      return ()
    }
    member __.ReadAll() = async{
      use fs = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)
      use r = new StreamReader(fs)
      let! lines = r.ReadToEndAsync()
      let map line=
        let p = FSharp.Data.JsonValue.Parse line
        let k: Command array ParseResult = ofJson p
        match k with
        | Ok line ->line
        | Error err->failwithf "Couldn't parse line %O" err //TODO: Fix IAppendBatch interface
      let splitLines (s:string)=s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
      return splitLines lines
              |> Array.collect map
              |> Array.toList
              |> List.sortBy Command.getAt
    }
