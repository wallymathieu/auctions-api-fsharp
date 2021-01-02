module Auctions.MapToRedis
open System.Collections.Generic
open System.Globalization
open Auctions.Domain

open StackExchange.Redis
open System
open FSharpPlus
open FSharpPlus.Data
open FSharp.Codecs.Redis
open FSharp.Codecs.Redis.Operators
type Typ = Domain.Type
module DateTime=
  let ticks (d:DateTime)=d.Ticks

let tryParseUser (u:RedisValue) : Result<User,_> = match string u |> tryParse with | Some v -> Ok v | None -> Decode.Fail.invalidValue u "InvalidUser"
let tryParseType (t:RedisValue) : Result<Typ,_> = match string t |> tryParse with | Some v -> Ok v | None -> Decode.Fail.invalidValue t "InvalidType"
let private dateTimeFormat = "yyyy-MM-ddTHH:mm:ss.fffZ"
let tryParseDateTime (t:RedisValue) : Result<DateTime,_> =
  let parsedAsString=
    match DateTime.TryParseExact ( string t, [| dateTimeFormat |], null, DateTimeStyles.RoundtripKind) with
    | true, d -> Ok d
    | _ -> Decode.Fail.invalidValue t "InvalidType"
  let parsedParsedAsInt _ =
    match tryParse <| string t with
    | Some v-> Ok <| DateTime v
    | _ -> Decode.Fail.invalidValue t "InvalidType"
  Result.bindError parsedParsedAsInt parsedAsString
let ofDateTime (d:DateTime) : RedisValue = implicit <| d.ToString dateTimeFormat
let timeAndAuctionCodec : ConcreteCodec<HashEntry list, HashEntry list, (DateTime*Auction),(DateTime*Auction)> =
  fun id title expiry startsAt at typ currency user -> (at, { id=AuctionId id;title=title; expiry =expiry; startsAt=startsAt; typ= typ; currency=Currency.ofValue currency; user= user })
  <!> rreq "Id" (snd >> Auction.getId >> AuctionId.unwrap >> Some)
  <*> rreq "Title" (snd >> Auction.title >> Some)
  <*> rreqWith (tryParseDateTime,ofDateTime) "Expiry" (snd >> Auction.expiry >> Some)
  <*> rreqWith (tryParseDateTime,ofDateTime) "StartsAt" (snd >> Auction.startsAt >> Some)
  <*> rreqWith (tryParseDateTime,ofDateTime) "At" (fst >> Some)
  <*> rreqWith (tryParseType, (string>>implicit)) "Typ" (snd >> Auction.typ >> Some)
  <*> rreq "Currency" (snd >> Auction.currency >> Currency.value >> Some)
  <*> rreqWith (tryParseUser, (string>>implicit)) "User" (snd >> Auction.user >> Some )
let timeAndBidCodec : ConcreteCodec<HashEntry list, HashEntry list, (DateTime*Bid),(DateTime*Bid)> =
  fun (id:string) auction amountValue amountCurrency at bidAt user -> (at, { id=Guid.Parse id |> BidId; auction=AuctionId auction; amount={value=amountValue; currency=Currency.ofValue amountCurrency}; user= user; at= Option.defaultValue at bidAt })
  <!> rreq "Id" (snd >> Bid.getId >> string >> Some)
  <*> rreq "Auction" (snd >> Bid.auction >> AuctionId.unwrap >> Some)
  <*> rreq "AmountValue" (snd >> Bid.amount >> Amount.value >> Some)
  <*> rreq "AmountCurrency" (snd >> Bid.amount >> Amount.currency >> Currency.value >> Some)
  <*> rreqWith (tryParseDateTime,ofDateTime) "At" (fst >> Some)
  // since BidAt is a new field, we don't want to require, we will use At if it's missing
  <*> roptWith (tryParseDateTime,ofDateTime) "BidAt" (snd >> Bid.at >> Some)
  <*> rreqWith (tryParseUser, (string>>implicit)) "User" (snd >> Bid.bidder >> Some)

/// tag redis codec with property and value
let inline tag prop value codec =
  let findEntry key (entries : HashEntry list) = entries |> List.tryFind (fun e -> e.Name.Equals(key))
  let toKV (he:HashEntry) = KeyValuePair (he.Name, he.Value)
  let k :RedisValue = implicit prop
  let v :RedisValue = implicit value
  let tag = HashEntry(implicit prop, implicit value)
  let matchPropValue (o:HashEntry list) =
       match findEntry k o with
       | Some a when a.Value = v -> Ok o
       | Some a -> Decode.Fail.invalidValue a.Value value
       | None -> Decode.Fail.propertyNotFound prop (List.map toKV o)
  Codec.ofConcrete codec
  |> Codec.compose (
                      matchPropValue,
                      fun encoded ->
                        if List.isEmpty encoded then empty // we have not encoded anything so no need to tag it
                        else encoded @ [ tag ]
                   )
  |> Codec.toConcrete
let inline withCase (toType:'a->'t) (ofType:'t->'a option) (codec:ConcreteCodec<HashEntry list,HashEntry list,'a,'a>) =
  let encode v = match ofType v with Some case' -> Codec.encode (Codec.ofConcrete codec) case' | None -> empty
  let decode = Codec.decode (Codec.ofConcrete codec) >> Result.map (toType)
  (decode,encode) |> Codec.toConcrete
/// to be able to persist commands
module Command =
  let codec : ConcreteCodec<HashEntry list,HashEntry list,Command,Command> =
    let addAuctionCodec = withCase AddAuction (function | AddAuction (a,b)-> Some (a,b) | _ -> None) timeAndAuctionCodec
    let placeBidCodec = withCase PlaceBid (function | PlaceBid (a,b)-> Some (a,b) | _ -> None) timeAndBidCodec
    (tag "Type" "AddAuction" addAuctionCodec) <|> (tag "Type" "PlaceBid" placeBidCodec)

  let mapToHashEntries command = Codec.encode (Codec.ofConcrete codec) command

  let mapFromHashEntries entries : Command =
    match Codec.decode (Codec.ofConcrete codec) entries with | Ok v -> v | Error err -> failwithf "Unknown %A" err

/// to be able to persist events
module Event =
  let codec : ConcreteCodec<HashEntry list,HashEntry list,Event,Event> =
    let addAuctionCodec = withCase AuctionAdded (function | AuctionAdded (a,b)-> Some (a,b) | _ -> None) timeAndAuctionCodec
    let placeBidCodec = withCase BidAccepted (function | BidAccepted (a,b)-> Some (a,b) | _ -> None) timeAndBidCodec
    (tag "Type" "AuctionAdded" addAuctionCodec) <|> (tag "Type" "BidAccepted" placeBidCodec)

  let mapToHashEntries event = Codec.encode (Codec.ofConcrete codec) event

  let mapFromHashEntries entries : Event =
    match Codec.decode (Codec.ofConcrete codec) entries with | Ok v -> v | Error err -> failwithf "Unknown %A" err

