module Auctions.Environment
open Suave
open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators

type DictEntry = System.Collections.DictionaryEntry

module Env=
  let vars ()=
    Seq.cast<DictEntry>( Environment.GetEnvironmentVariables())
    |> Seq.map(fun kv-> (string kv.Key, string kv.Value) )
  let envArgs (prefix:string) (env:(string*string) seq) =
    let mangle (str:string) = Regex.replace "_" "-" str
    env
    |> Seq.filter( fun (key, value) -> key.StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase)
                                       && not <| String.IsNullOrEmpty value)
    |> Seq.collect( fun (key, value) -> [ "--"+ (mangle (key.Substring(prefix.Length))).ToLowerInvariant(); value ])
    |> Seq.toList
