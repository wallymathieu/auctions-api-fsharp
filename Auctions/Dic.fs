module Auctions.Dic
open System.Collections.Generic


let tryGet k (c : IDictionary<_, _>) = 
  match c.TryGetValue(k) with
  | true, value -> Some(value)
  | false, _ -> None

