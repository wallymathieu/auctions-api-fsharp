module Auctions.Dic
open System.Collections.Generic


let tryGet k (c : IDictionary<_, _>) = 
  match c.TryGetValue(k) with
  | true, value -> Some(value)
  | false, _ -> None

let addOrUpdate k (add) (update) (c : IDictionary<_, _>) =
  match c.TryGetValue(k) with
  | true, value -> 
    let updated =update(value)
    c.[k] <- updated
    updated
  | false, _ -> 
    c.Add(k, add)
    add