module Tests.DomainJsonSerialization
open System

open System.Globalization
open Auctions.Domain
open Auctions

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
[<Fact>]
let ``Bid can be deserialized correctly``() =
  let bidJson = """{"id":"8f6e4c445ee443a49006a0f8f3a04ba1","auction":1,"user":"BuyerOrSeller|a2|Buyer","amount":"VAC11","at":"2020-05-17T08:05:59.171Z"}"""
  let b : Bid ParseResult = parseJson bidJson
  match b with
  | Ok bid->
    Assert.Equal (BidId <| Guid.Parse "8f6e4c445ee443a49006a0f8f3a04ba1", bid.id)
    Assert.Equal (AuctionId <| 1L, bid.auction)
    Assert.Equal (BuyerOrSeller (UserId "a2","Buyer"), bid.user)
    Assert.Equal (Amount.Parse "VAC11", bid.amount)
    let expectedAt = DateTime.ParseExact ("2020-05-17T08:05:59.171Z", [| "yyyy-MM-ddTHH:mm:ss.fffZ"; |], null, DateTimeStyles.RoundtripKind)
    Assert.Equal (expectedAt, bid.at)
  | Error e -> failwith "%A" e
[<Fact>]
let ``Auction can be deserialized correctly``() =
  let bidJson = """{"id":2,"startsAt":"2018-12-01T10:00:00.000Z","title":"Some auction","expiry":"2020-05-18T10:00:00.000Z","user":"BuyerOrSeller|a1|Test","type":"English|VAC0|VAC0|0","currency":"VAC"}"""
  let a : Auction ParseResult = parseJson bidJson
  match a with
  | Ok auction->
    Assert.Equal (AuctionId <| 2L, auction.id)
    Assert.Equal (BuyerOrSeller (UserId "a1","Test"), auction.user)
    let expectedAt = DateTime.ParseExact ("2018-12-01T10:00:00.000Z", [| "yyyy-MM-ddTHH:mm:ss.fffZ"; |], null, DateTimeStyles.RoundtripKind)
    Assert.Equal (expectedAt, auction.startsAt)
    Assert.Equal ("Some auction", auction.title)
    Assert.Equal (Currency.VAC, auction.currency)
    Assert.Equal (Type.Parse "English|VAC0|VAC0|0", auction.typ)
  | Error e -> failwith "%A" e

let inline roundtripEq (isEq: 'a -> 'a -> bool) p =
    let actual = p |> toJson |> ofJson
    let ok =
        match actual with
        | Ok actual -> isEq actual p
        | _ -> false
    if not ok then printfn "Got %A from %A" actual p
    ok

let inline roundtrip p = roundtripEq (=) p

[<Property>]
let ``serialized datetime is the same as the input`` (d: DateTime) = roundtrip d
[<Property>]
let ``serialized Currency is the same as the input`` (u: Currency) = roundtrip u
(*[<Property>]
let ``serialized User is the same as the input`` (NonNull id) (NonNull name) = roundtrip (BuyerOrSeller (UserId id,name)) *)
[<Property>]
let ``serialized BidId is the same as the input`` (u: BidId) = roundtrip u
[<Property>]
let ``serialized AuctionId is the same as the input`` (u: AuctionId) = roundtrip u
[<Property>]
let ``serialized Amount is the same as the input`` (PositiveInt u) c = roundtrip ({ value =int64 u; currency=c })
(*[<Property>]
let ``serialized Type is the same as the input`` (u: Type) = roundtrip u *)
(*[<Property>]
let ``serialized bids are the same as the input bids`` id a user (PositiveInt u) c (PositiveInt at)=
  roundtrip { id=id; auction=a; user=user; amount={ value =int64 u; currency=c }; at=DateTime (int64 at) } *)
(*[<Property>]
let ``serialized auctions are the same as the input auctions`` (auction: Auction) = roundtrip auction *)
(*[<Property>]
let ``serialized commands are the same as the input commands`` (command: Command) = roundtrip command *)
(*[<Property>]
let ``serialized events are the same as the input commands`` (event: Event) = roundtrip event *)

