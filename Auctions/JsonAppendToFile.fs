namespace Auctions
open Auctions.Domain

open System.IO
open System
open Fleece
open Fleece.FSharpData


type JsonAppendToFile(fileName) = 
  let notNull= not << isNull
  
  do
    if not <| File.Exists fileName then File.WriteAllText(fileName, "")
    else 
        ()


  interface IAppendBatch with
    
    member __.Batch cs = 
      use fs = File.Open(fileName, FileMode.Append, FileAccess.Write, FileShare.Read)
      use w = new StreamWriter(fs)
      let json = toJson cs
      json.WriteTo (w,FSharp.Data.JsonSaveOptions.DisableFormatting)
      fs.Flush()
    
    member __.ReadAll() = 
      use fs = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)
      use r = new StreamReader(fs)
      seq { 
        let line = ref ""
        
        let readLine() = 
          line := r.ReadLine()
          line.Value
        while (notNull <| readLine()) do
          let p = FSharp.Data.JsonValue.Parse line.Value
          let k: Command array ParseResult = ofJson p
          match k with
          | Ok line ->yield line
          | Error err->failwithf "Couldn't parse line %O" err //TODO: Fix IAppendBatch interface
      }
      |> Seq.concat
      |> Seq.toList
      |> List.sortBy Command.getAt
