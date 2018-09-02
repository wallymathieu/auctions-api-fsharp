module Auctions.MapToRedis
open Auctions.Domain

open StackExchange.Redis
open System
open FSharpPlus
open FSharpPlus.Data

module Redis=
  
  // Codec definition From Fleece:

  /// Encodes a value of a generic type 't into a value of raw type 'S.
  type Encoder<'S, 't> = 't -> 'S 

  /// Decodes a value of raw type 'S into a value of generic type 't, possibly returning an error.
  type Decoder<'S, 't> = 'S -> Result<'t,string>

  /// A decoder from raw type 'S1 and encoder to raw type 'S2 for string types 't1 and 't2.
  type Codec<'S1, 'S2, 't1, 't2> = Decoder<'S1, 't1> * Encoder<'S2, 't2>

  /// A decoder from raw type 'S1 and encoder to raw type 'S2 for type 't.
  type Codec<'S1, 'S2, 't> = Codec<'S1, 'S2, 't, 't>

  /// A codec for raw type 'S decoding to strong type 't1 and encoding to strong type 't2.
  type SplitCodec<'S, 't1, 't2> = Codec<'S, 'S, 't1, 't2>

  /// A codec for raw type 'S to strong type 't.
  type Codec<'S, 't> = Codec<'S, 'S, 't>

  let decode (d: Decoder<'i, 'a>) (i: 'i) : Result<'a,_> = d i
  let encode (e: Encoder<'o, 'a>) (a: 'a) : 'o = e a

  /// <summary>Initialize the field mappings.</summary>
  /// <param name="f">An object initializer as a curried function.</param>
  /// <returns>The resulting object codec.</returns>
  let mapping f = (fun _ -> Ok f), (fun _ -> [])
  let inline get fromRedis (o: HashEntry list) (key:string) =
    let k = implicit key
    match o |> List.tryFind (fun p -> p.Name.Equals(k) ) with
    | Some v-> fromRedis v.Value
    | None -> failwithf "Could not find %s" key

  let diApply combiner toBC (remainderFields: SplitCodec<'S, 'f ->'r, 'T>) (currentField: Codec<'S, 'f>) = //NOTE: From Fleece
      (
          Compose.run (Compose (fst remainderFields: Decoder<'S, 'f -> 'r>) <*> Compose (fst currentField)),
          toBC >> (encode (snd currentField) *** encode (snd remainderFields)) >> combiner
      )

  /// <summary>Appends a field mapping to the codec.</summary>
  /// <param name="fieldName">A string that will be used as key to the field.</param>
  /// <param name="getter">The field getter function.</param>
  /// <param name="rest">The other mappings.</param>
  /// <param name="fromRedis">Map from redis value to value.</param>
  /// <param name="toRedis">Map to redis value.</param>
  /// <returns>The resulting object codec.</returns>
  let inline field fieldName fromRedis toRedis (getter: 'T -> 'Value) (rest: SplitCodec<_, _->'Rest, _>) = //NOTE: From Fleece
      let inline deriveFieldCodec prop =
          (
              (fun (o: HashEntry list) -> get fromRedis o prop),
              (fun (x: 't) -> [HashEntry(implicit prop, toRedis x)])
          )
      diApply (List.append |> uncurry) (fanout getter id) rest (deriveFieldCodec fieldName)
  let fieldStr fieldName getter rest =
    field fieldName (string>>Ok) implicit getter rest
  let fieldInt64 fieldName getter rest =
    let getInt64 (v:RedisValue)=
      let intV = ref 0L
      if v.TryParse intV then Ok (intV.Value)
      else Error (sprintf "Could not parse %O" v)
    field fieldName getInt64 implicit getter rest
module DateTime=
  let ticks (d:DateTime)=d.Ticks
open Redis
let addAuctionCodec =
  fun id title expiry startsAt at typ currency user -> (DateTime at, { id=id;title=title; expiry =DateTime expiry; startsAt=DateTime startsAt; typ=Type.__parse typ; currency=Currency.ofValue currency; user=User.__parse user })
  |> mapping
  |> fieldInt64 "Id" (snd >> Auction.getId)
  |> fieldStr "Title" (snd >> Auction.title)
  |> fieldInt64 "Expiry" (snd >> Auction.expiry >> DateTime.ticks)
  |> fieldInt64 "StartsAt" (snd >> Auction.expiry >> DateTime.ticks)
  |> fieldInt64 "At" (fst >> DateTime.ticks)
  |> fieldStr "Typ" (snd >> Auction.typ >> string)
  |> fieldInt64 "Currency" (snd >> Auction.currency >> Currency.value)
  |> fieldStr "User" (snd >> Auction.user >> string)
let placeBidCodec =
  fun id auction amountValue amountCurrency at user -> (DateTime at, { id=Guid.Parse id; auction=auction; amount={value=amountValue; currency=Currency.ofValue amountCurrency}; user=User.__parse user; at=DateTime at })
  |> mapping
  |> fieldStr "Id" (snd >> Bid.getId >> string)
  |> fieldInt64 "Auction" (snd >> Bid.auction)
  |> fieldInt64 "AmountValue" (snd >> Bid.amount >> Amount.value)
  |> fieldInt64 "AmountCurrency" (snd >> Bid.amount >> Amount.currency >> Currency.value)
  |> fieldInt64 "At" (fst >> DateTime.ticks)
  |> fieldStr "User" (snd >> Bid.bidder >>  string)

let mapToHashEntries command =
  let hashEntryStr (key:string) (value:string) = HashEntry(implicit key, implicit value)
  let withType t xs = hashEntryStr "Type" t :: xs
  match command with
  | AddAuction(at, auction)-> encode (snd addAuctionCodec) (at, auction) |> withType "AddAuction"
  | PlaceBid(at, bid) ->  encode (snd placeBidCodec) (at, bid) |> withType "PlaceBid"

let findEntry (key:string) (entries : HashEntry list) = 
  let k = implicit key
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
    match decode (fst addAuctionCodec) entries with
    | Ok (at,auction) -> AddAuction (at,auction)
    | Error err-> failwithf "Unknown %A" err
  | "PlaceBid" -> 
    match decode (fst placeBidCodec) entries with
    | Ok (at,bid) -> PlaceBid(at, bid)
    | Error err-> failwithf "Unknown %A" err
  | v -> failwithf "Unknown type %s" v
 
