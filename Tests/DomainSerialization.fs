module Tests.DomainSerialization
open System

open System.Globalization
open Auctions.Domain

open Xunit
open FsCheck
open FsCheck.Xunit
open Fleece
open Fleece.FSharpData

let sampleJsonLines = """
[{"$type":"AddAuction","at":"2020-05-17T08:05:54.943Z","auction":{"id":2,"startsAt":"2018-12-01T10:00:00.000Z","title":"Some auction","expiry":"2020-05-18T10:00:00.000Z","user":"BuyerOrSeller|a1|Test","type":"English|0|0|0","currency":"VAC","open": false}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:05:59.182Z","bid":{"auction":1,"user":"BuyerOrSeller|a2|Buyer","amount":11,"at":"2020-05-17T08:05:59.171Z"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:02.198Z","bid":{"auction":2,"user":"BuyerOrSeller|a2|Buyer","amount":11,"at":"2020-05-17T08:06:02.197Z"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:06.854Z","bid":{"auction":2,"user":"BuyerOrSeller|a1|Test","amount":11,"at":"2020-05-17T08:06:06.854Z"}}]
[{"$type":"AddAuction","at":"2020-05-17T08:06:37.128Z","auction":{"id":1,"startsAt":"2018-12-01T10:00:00.000Z","title":"Some auction","expiry":"2020-05-18T10:00:00.000Z","user":"BuyerOrSeller|a1|Test","type":"English|0|0|0","currency":"VAC","open": false}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:53.148Z","bid":{"auction":1,"user":"BuyerOrSeller|a2|Buyer","amount":11,"at":"2020-05-17T08:06:53.147Z"}}]
[{"$type":"PlaceBid","at":"2020-05-17T08:06:57.773Z","bid":{"auction":1,"user":"BuyerOrSeller|a1|Test","amount":11,"at":"2020-05-17T08:06:57.773Z"}}]
"""
let parseCommands lines =
  let parseLine line=
    let k : Command array ParseResult = ofJsonText line
    match k with
    | Ok line -> line
    | Error err->failwithf $"Couldn't parse line due to error:\n%A{err}\n for line\n%s{line}"
  let splitLines (s:string)=s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
  splitLines lines |> Array.collect parseLine |> Array.toList

module Json =
  open FSharp.Data
  open Json

  [<Fact>]
  let ``Can deserialize existing commands``() = parseCommands sampleJsonLines |> ignore
  [<Fact>]
  let ``commands sample json can be deserialized and serialized to the same json``() =
    let parseLine line=
      let k : Command array ParseResult = ofJsonText line
      match k with
      | Ok commands ->
        let j = toJsonValue commands
        assertJsonEqual (JsonValue.Parse line, j)
      | Error err -> failwithf $"Couldn't parse line due to error:\n%A{err}\n for line\n%s{line}"
    let splitLines (s:string)=s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
    splitLines sampleJsonLines |> Array.iter parseLine

  let bidJson = """{"auction":1,"user":"BuyerOrSeller|a2|Buyer","amount":11,"at":"2020-05-17T08:05:59.171Z"}"""
  [<Fact>]
  let ``Bid sample json can be deserialized correctly``() =
    let b : Bid ParseResult = ofJsonText bidJson
    match b with
    | Ok bid->
      Assert.Equal (AuctionId <| 1L, bid.auction)
      Assert.Equal (BuyerOrSeller (UserId "a2","Buyer"), bid.user)
      Assert.Equal (11L, bid.amount)
      let expectedAt = DateTime.ParseExact ("2020-05-17T08:05:59.171Z", [| "yyyy-MM-ddTHH:mm:ss.fffZ"; |], null, DateTimeStyles.RoundtripKind)
      Assert.Equal (expectedAt, bid.at)
    | Error e -> failwithf $"Error %A{e}"
  [<Fact>]
  let ``Bid sample json can be deserialized and serialized to the same json``() =
    let b : Bid ParseResult = ofJsonText bidJson
    match b with
    | Ok bid->
      let j = toJsonValue bid
      assertJsonEqual (JsonValue.Parse bidJson, j)
    | Error e -> failwithf $"Error %A{e}"

  let auctionJson = """{"id":2,"startsAt":"2018-12-01T10:00:00.000Z","title":"Some auction","expiry":"2020-05-18T10:00:00.000Z","user":"BuyerOrSeller|a1|Test","type":"English|0|0|0","currency":"VAC","open": false}"""
  [<Fact>]
  let ``Auction sample json can be deserialized correctly``() =
    let a : Auction ParseResult = ofJsonText auctionJson
    match a with
    | Ok auction->
      Assert.Equal (AuctionId <| 2L, auction.id)
      Assert.Equal (BuyerOrSeller (UserId "a1","Test"), auction.user)
      let expectedAt = DateTime.ParseExact ("2018-12-01T10:00:00.000Z", [| "yyyy-MM-ddTHH:mm:ss.fffZ"; |], null, DateTimeStyles.RoundtripKind)
      Assert.Equal (expectedAt, auction.startsAt)
      Assert.Equal ("Some auction", auction.title)
      Assert.Equal (Currency.VAC, auction.currency)
      Assert.Equal (Type.Parse "English|0|0|0", auction.typ)
    | Error e -> failwithf $"Error %A{e}"
  [<Fact>]
  let ``Auction sample json can be deserialized and serialized to the same json``() =
    let a : Auction ParseResult = ofJsonText auctionJson
    match a with
    | Ok auction->
      let j = toJsonValue auction
      assertJsonEqual (JsonValue.Parse auctionJson, j)
    | Error e -> failwithf $"Error %A{e}"

  let inline roundtripEq (isEq: 'a -> 'a -> bool) p =
      let actual = p |> toJson |> ofJson
      let ok =
          match actual with
          | Ok actual -> isEq actual p
          | _ -> false
      if not ok then printfn $"Got %A{actual} from %A{p}"
      ok

  let inline roundtrip p = roundtripEq (=) p

  [<Property>]
  let ``serialized datetime is the same as the input`` (d: DateTime) = roundtrip d
  [<Property>]
  let ``serialized Currency is the same as the input`` (u: Currency) = roundtrip u
  [<Fact>]
  let ``serialized Buyer or seller User is the same as the input``() =
    fsCheck (Prop.forAll Arb.buyerOrSeller roundtrip)
  [<Fact>]
  let ``serialized Support User is the same as the input``() =
    fsCheck (Prop.forAll Arb.support roundtrip)
  [<Property>]
  let ``serialized AuctionId is the same as the input`` (u: AuctionId) = roundtrip u
  [<Property>]
  let ``serialized Amount is the same as the input`` (PositiveInt u) c = roundtrip { value =int64 u; currency=c }
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

module Redis =
  open Auctions.MapToRedis
  [<Fact>]
  let ``sample commands can be deserialized and serialized to the same values``() =
    let commands = parseCommands sampleJsonLines
    for command in commands do
       let redisValue = Command.mapToHashEntries command
       let deserialized = Command.mapFromHashEntries redisValue
       match command,deserialized with

       | PlaceBid (at1,{auction=auctionId1;user=user1;amount=amount1;at=at1_1}),
          PlaceBid (at2,{auction=auctionId2;user=user2;amount=amount2;at=at1_2}) ->
         Assert.Equal (at1,at2)
         Assert.Equal (auctionId1,auctionId2)
         Assert.Equal (user1,user2)
         Assert.Equal (amount1,amount2)
         Assert.Equal (at1_1, at1_2)
       | AddAuction (at1,auction1), AddAuction (at2,auction2) ->
         Assert.Equal (at1,at2)
         Assert.Equal (auction1,auction2)
       | _, _ -> Assert.True ((command = deserialized), $"Expected:\n%A{command}\nto equal:\n%A{deserialized}")
