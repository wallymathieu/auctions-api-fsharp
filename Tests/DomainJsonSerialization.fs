module Tests.DomainJsonSerialization

open Auctions.Domain

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Fleece.FSharpData

let sampleLines = """
[{"$type":"AddAuction","at":"2020-05-17T08:05:54.943Z","auction":{"id":2,"startsAt":"2018-12-01T10:00:00.000Z","title":"Some auction","expiry":"2020-05-18T10:00:00.000Z","user":"BuyerOrSeller|a1|Test","type":"English|VAC0|VAC0|0","currency":"VAC"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:05:59.182Z","bid":{"id":"8f6e4c445ee443a49006a0f8f3a04ba1","auction":1,"user":"BuyerOrSeller|a2|Buyer","amount":"VAC11","at":"2020-05-17T08:05:59.171Z"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:02.198Z","bid":{"id":"3c2a3daf41a649b3bc02e5f431690276","auction":2,"user":"BuyerOrSeller|a2|Buyer","amount":"VAC11","at":"2020-05-17T08:06:02.197Z"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:06.854Z","bid":{"id":"4002b05b87da4ba5af00557c27033568","auction":2,"user":"BuyerOrSeller|a1|Test","amount":"VAC11","at":"2020-05-17T08:06:06.854Z"}}]
[{"$type":"AddAuction","at":"2020-05-17T08:06:37.128Z","auction":{"id":1,"startsAt":"2018-12-01T10:00:00.000Z","title":"Some auction","expiry":"2020-05-18T10:00:00.000Z","user":"BuyerOrSeller|a1|Test","type":"English|VAC0|VAC0|0","currency":"VAC"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:53.148Z","bid":{"id":"34edc64399b442be89598e1d9f577350","auction":1,"user":"BuyerOrSeller|a2|Buyer","amount":"VAC11","at":"2020-05-17T08:06:53.147Z"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:57.773Z","bid":{"id":"da03e144d59d4cc2bbb484aee951cc31","auction":1,"user":"BuyerOrSeller|a1|Test","amount":"VAC11","at":"2020-05-17T08:06:57.773Z"}}]
"""

[<Fact>]
let ``Can deserialize existing commands``() =
  let parseLine line=
    let k : Command array ParseResult = parseJson line
    match k with
    | Ok _ ->()
    | Error err->failwithf "Couldn't parse line due to error: %O, for line %s" err line
  let splitLines (s:string)=s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
  splitLines sampleLines
            |> Array.iter parseLine

[<Property>]
let ``serialized bids are the same as the input bids`` (bid: Bid) =
  let json = toJson bid |> string
  let deserialized = parseJson json
  (Ok bid) = deserialized
[<Property>]
let ``serialized auctions are the same as the input auctions`` (auction: Auction) =
  let json = toJson auction |> string
  let deserialized = parseJson json
  (Ok auction) = deserialized
[<Property>]
let ``serialized commands are the same as the input commands`` (command: Command) =
  let json = toJson auction |> string
  let deserialized = parseJson json
  (Ok command) = deserialized
[<Property>]
let ``serialized events are the same as the input commands`` (event: Event) =
  let json = toJson auction |> string
  let deserialized = parseJson json
  (Ok event) = deserialized

