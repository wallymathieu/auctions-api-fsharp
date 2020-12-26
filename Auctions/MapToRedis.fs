module Auctions.MapToRedis
open Auctions.Domain

open StackExchange.Redis
open System
open FSharpPlus
open FSharpPlus.Data
open FSharp.Codecs.Redis
open FSharp.Codecs.Redis.Operators
type Typ = Domain.Auctions.Type
module DateTime=
  let ticks (d:DateTime)=d.Ticks
open Redis
let tryParseUser (u:RedisValue) : Result<User,_> = match string u |> tryParse with | Some v -> Ok v | None -> Decode.Fail.invalidValue u "InvalidUser"
let tryParseType (t:RedisValue) : Result<Typ,_> = match string t |> tryParse with | Some v -> Ok v | None -> Decode.Fail.invalidValue t "InvalidType"
let addAuctionCodec =
  fun id title expiry startsAt at typ currency user -> (DateTime at, { id=AuctionId id;title=title; expiry =DateTime expiry; startsAt=DateTime startsAt; typ= typ; currency=Currency.ofValue currency; user= user })
  <!> rreq "Id" (snd >> Auction.getId >> AuctionId.unwrap >> Some)
  <*> rreq "Title" (snd >> Auction.title >> Some)
  <*> rreq "Expiry" (snd >> Auction.expiry >> DateTime.ticks >> Some)
  <*> rreq "StartsAt" (snd >> Auction.startsAt >> DateTime.ticks >> Some)
  <*> rreq "At" (fst >> DateTime.ticks >> Some)
  <*> rreqWith (tryParseType, (string>>implicit)) "Typ" (snd >> Auction.typ >> Some)
  <*> rreq "Currency" (snd >> Auction.currency >> Currency.value >> Some)
  <*> rreqWith (tryParseUser, (string>>implicit)) "User" (snd >> Auction.user >> Some )
let placeBidCodec =
  fun (id:string) auction amountValue amountCurrency at user -> (DateTime at, { id=Guid.Parse id |> BidId; auction=AuctionId auction; amount={value=amountValue; currency=Currency.ofValue amountCurrency}; user= user; at=DateTime at })
  <!> rreq "Id" (snd >> Bid.getId >> string >> Some)
  <*> rreq "Auction" (snd >> Bid.auction >> AuctionId.unwrap >> Some)
  <*> rreq "AmountValue" (snd >> Bid.amount >> Amount.value >> Some)
  <*> rreq "AmountCurrency" (snd >> Bid.amount >> Amount.currency >> Currency.value >> Some)
  <*> rreq "At" (fst >> DateTime.ticks >> Some)
  <*> rreqWith (tryParseUser, (string>>implicit)) "User" (snd >> Bid.bidder >> Some)

let mapToHashEntries command =
  let hashEntryStr (key:string) (value:string) = HashEntry(implicit key, implicit value)
  let withType t xs = hashEntryStr "Type" t :: xs
  match command with
  | AddAuction(at, auction)-> Codec.encode (Codec.ofConcrete addAuctionCodec) (at, auction) |> withType "AddAuction"
  | PlaceBid(at, bid) ->  Codec.encode (Codec.ofConcrete placeBidCodec) (at, bid) |> withType "PlaceBid"

let findEntry (key:string) (entries : HashEntry list) =
  let k :RedisValue= implicit key
  match entries |> List.tryFind (fun e -> e.Name.Equals(k)) with
  | Some entry -> entry
  | None -> failwithf "could not find %s" key

let findEntryStr key entries =
  let entry = findEntry key entries
  string entry.Value

let mapFromHashEntries entries : Command =
  let t = entries |> findEntryStr "Type"
  match t with
  | "AddAuction" ->
    match Codec.decode (Codec.ofConcrete addAuctionCodec) entries with
    | Ok (at,auction) -> AddAuction (at,auction)
    | Error err-> failwithf "Unknown %A" err
  | "PlaceBid" ->
    match Codec.decode (Codec.ofConcrete placeBidCodec) entries with
    | Ok (at,bid) -> PlaceBid(at, bid)
    | Error err-> failwithf "Unknown %A" err
  | v -> failwithf "Unknown type %s" v

