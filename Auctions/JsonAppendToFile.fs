namespace Auctions
open Auctions.Domain

open System.IO
open System
open FSharp.Data
open Fleece
open Fleece.FSharpData

module JsonAppendToFile =
  let batch fileName (toJson:'cs -> JsonValue) cs = async{
    use fs = File.Open(fileName, FileMode.Append, FileAccess.Write, FileShare.Read)
    use w = new StreamWriter(fs)
    let json = toJson cs
    json.WriteTo (w, JsonSaveOptions.DisableFormatting)
    do! w.WriteLineAsync()
    do! fs.FlushAsync()
    return ()
  }
  let readAll (parseJson: string -> 'c array ParseResult) getAt fileName = async{
    use fs = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)
    use r = new StreamReader(fs)
    let! lines = r.ReadToEndAsync()
    let map line=
      let k = parseJson line
      match k with
      | Ok line ->line
      | Error err->failwithf "Couldn't parse line %O" err // not an expected error
    let splitLines (s:string)=s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
    return splitLines lines
            |> Array.collect map
            |> Array.toList
            |> List.sortBy getAt
  }

type JsonAppendToFile<'T>(fileName, toJson, parseJson, getAt:'T -> DateTime) =
  let fileDoesNotExist = not << File.Exists
  do
    if fileDoesNotExist fileName then File.WriteAllText(fileName, "")

  interface IAppendBatch<'T> with
    member __.Batch cs = JsonAppendToFile.batch fileName toJson cs
    member __.ReadAll() = JsonAppendToFile.readAll parseJson getAt fileName

type JsonAppendEventToFile(fileName) =
  inherit JsonAppendToFile<Event> (fileName, toJsonValue, ofJsonText, Event.getAt)
type JsonAppendCommandToFile(fileName) =
  inherit JsonAppendToFile<Command> (fileName, toJsonValue, ofJsonText, Command.getAt)
