module Auctions.MapToRedis
open StackExchange.Redis
open System
open Domain
open Commands

let redisKey (str : string) = RedisKey.op_Implicit (str)
let redisValueStr (str : string) = RedisValue.op_Implicit str
let redisValueInt64 (v : int64) = RedisValue.op_Implicit v
let redisValueFloat (v : float) = RedisValue.op_Implicit v
let valueToKey (v : RedisValue) : RedisKey = RedisKey.op_Implicit (v.ToString())
let hashEntryStr key value = new HashEntry(redisValueStr key, redisValueStr value)
let hashEntryInt64 key value = new HashEntry(redisValueStr key, redisValueInt64 value)
let hashEntryFloat key value = new HashEntry(redisValueStr key, redisValueFloat value)

let mapToHashEntries command = 
  let withType t xs = hashEntryStr "Type" t :: xs
  match command with
  | Empty(at = at) -> 
    [ hashEntryInt64 "At" at.Ticks ]
    |> withType "Empty"
  | AddAuction (at = at;auction=auction) -> 
    [ hashEntryInt64 "Id" (auction.id)
      hashEntryStr "Title" (auction.title)
      hashEntryInt64 "EndsAt" auction.endsAt.Ticks
      hashEntryInt64 "StartsAt" auction.startsAt.Ticks
      hashEntryInt64 "At" at.Ticks ]
    |> withType "AddAuction"
  | PlaceBid (bid) ->
    [ hashEntryStr "Id" (bid.id.ToString())
      hashEntryInt64 "Auction" (bid.auction)
      hashEntryFloat "AmountValue" bid.amount.value
      hashEntryStr "AmountCurrency" (bid.amount.currency.ToString())
      hashEntryInt64 "At" bid.at.Ticks
      hashEntryStr "User" (bid.user.ToString()) ]
    |> withType "PlaceBid"

let findEntry key (entries : HashEntry array) = 
  let k = redisValueStr key
  let entry = entries |> Array.find (fun e -> e.Name.Equals(k))
  entry

let findEntryStr key entries = 
  let entry = findEntry key entries
  entry.Value.ToString()

let findEntryInt64 key entries : int64 = 
  let entry = findEntry key entries
  match Int64.TryParse(entry.Value.ToString()) with
  | true, v -> v
  | false, _ -> failwithf "unable to parse value %A" entry.Value

let findEntryFloat key entries = 
  let entry = findEntry key entries
  match Double.TryParse(entry.Value.ToString()) with
  | true, v -> v
  | false, _ -> failwithf "unable to parse value %A" entry.Value

let mapFromHashEntries entries : Command = 
  let t = entries |> findEntryStr "Type"
  match t with
  | "Empty" -> 
    let at = 
      entries
      |> findEntryInt64 "At"
      |> DateTime
    Empty(at = at)
  | "AddAuction" -> 
    let id = 
      entries
      |> findEntryInt64 "Id"
      
    let title = entries |> findEntryStr "Title"
    
    let endsAt = 
      entries
      |> findEntryInt64 "EndsAt"
      |> DateTime

    let startsAt = 
      entries
      |> findEntryInt64 "StartsAt"
      |> DateTime

    let at = 
      entries
      |> findEntryInt64 "At"
      |> DateTime
    
    let user = 
      entries
      |> findEntryStr "User"
      |> Domain.User.parse
    let auction:Auction={id = id; title = title; startsAt = startsAt; endsAt = endsAt; user = user}
    AddAuction(at = at, auction=auction)
  | "PlaceBid" -> 
    let id = 
      entries
      |> findEntryStr "Id"
      |> Domain.BidId.Parse
    
    let auction = 
      entries
      |> findEntryInt64 "Auction"
    
    let amount = entries |> findEntryFloat "AmountValue"
    let currency = entries |> findEntryStr "AmountCurrency"
    
    let user = 
      entries
      |> findEntryStr "User"
      |> Domain.User.parse
    
    let at = 
      entries
      |> findEntryInt64 "At"
      |> DateTime
    let bid :Bid= { id = id; auction = auction
                    amount = { value = amount; currency = currency }
                    user = user; at = at
                  }
    PlaceBid(bid=bid)
  | v -> failwithf "Unknown type %s" v