namespace Auctions
open Auctions.Domain

open System.IO
open System
open Fleece
open Fleece.FSharpData
open FSharp.Data
open V2

type JsonAppendToFile(fileName) =
  let fileDoesNotExist = not << File.Exists
  do
    if fileDoesNotExist fileName then File.WriteAllText(fileName, "")

  interface IAppendBatch with
    member __.Batch cs = async{
      use fs = File.Open(fileName, FileMode.Append, FileAccess.Write, FileShare.Read)
      use w = new StreamWriter(fs)
      let json = toJson (List.map Event cs)
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
        let k: EventV2 array ParseResult = parseJson line
        match k with
        | Ok line -> line
        | Error err->failwithf "Couldn't parse line %O" err //TODO: Fix IAppendBatch interface
      let splitLines (s:string)=s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
      return splitLines lines
              |> Array.collect map
              |> Array.map EventV2.Unwrap
              |> Array.toList
              |> List.sortBy Event.getAt
    }
