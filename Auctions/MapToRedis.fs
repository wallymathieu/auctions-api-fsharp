module Auctions.MapToRedis
open Auctions.Domain

open StackExchange.Redis
open System

let redisKey (str : string) = RedisKey.op_Implicit (str)
let redisValueStr (str : string) = RedisValue.op_Implicit str
let redisValueInt64 (v : int64) = RedisValue.op_Implicit v
let redisValueFloat (v : float) = RedisValue.op_Implicit v
let valueToKey (v : RedisValue) : RedisKey = RedisKey.op_Implicit (string v)
let hashEntryStr key value = HashEntry(redisValueStr key, redisValueStr value)
let hashEntryInt64 key value = HashEntry(redisValueStr key, redisValueInt64 value)
let hashEntryFloat key value = HashEntry(redisValueStr key, redisValueFloat value)

let mapToHashEntries command = 
  let withType t xs = hashEntryStr "Type" t :: xs
  match command with
  | AddAuction(at, auction) -> 
    [ hashEntryInt64 "Id" (auction.id)
      hashEntryStr "Title" (auction.title)
      hashEntryInt64 "Expiry" auction.expiry.Ticks
      hashEntryInt64 "StartsAt" auction.startsAt.Ticks
      hashEntryInt64 "At" at.Ticks 
      hashEntryStr "Typ" (string auction.typ)
      hashEntryInt64 "Currency" (Currency.value auction.currency)
      hashEntryStr "User" (string auction.user) ]
    |> withType "AddAuction"
  | PlaceBid(at, bid) -> 
    [ hashEntryStr "Id" (string bid.id)
      hashEntryInt64 "Auction" (bid.auction)
      hashEntryInt64 "AmountValue" bid.amount.value
      hashEntryInt64 "AmountCurrency" (Currency.value bid.amount.currency)
      hashEntryInt64 "At" at.Ticks
      hashEntryStr "User" (string bid.user) ]
    |> withType "PlaceBid"

let findEntry key (entries : HashEntry array) = 
  let k = redisValueStr key
  let entry = entries |> Array.tryFind (fun e -> e.Name.Equals(k))
  match entry with
  | Some entry -> entry
  | None -> failwithf "could not find %s" key

let findEntryStr key entries = 
  let entry = findEntry key entries
  string entry.Value

let findEntryInt64 key entries : int64 = 
  let entry = findEntry key entries
  match Int64.TryParse(string entry.Value) with
  | true, v -> v
  | false, _ -> failwithf "unable to parse value %A" entry.Value

let findEntryFloat key entries = 
  let entry = findEntry key entries
  match Double.TryParse(string entry.Value) with
  | true, v -> v
  | false, _ -> failwithf "unable to parse value %A" entry.Value

let mapFromHashEntries entries : Command = 
  let t = entries |> findEntryStr "Type"
  match t with
  | "AddAuction" -> 
    let id = entries |> findEntryInt64 "Id"
    let title = entries |> findEntryStr "Title"
    
    let expiry = 
      entries
      |> findEntryInt64 "Expiry"
      |> DateTime
    
    let startsAt = 
      entries
      |> findEntryInt64 "StartsAt"
      |> DateTime
    
    let at = 
      entries
      |> findEntryInt64 "At"
      |> DateTime

    let currency = 
      entries
      |> findEntryInt64 "Currency"
      |> Currency.ofValue
    
    let user = 
      entries
      |> findEntryStr "User"
      |> User.tryParse

    if user.IsNone then failwith "missing user in auction data!"

    let typ = 
      entries
      |> findEntryStr "Typ"
      |> Domain.Auctions.Type.tryParse

    let auction : Auction = 
      { id = id
        title = title
        startsAt = startsAt
        expiry = expiry
        user = user.Value 
        currency = currency
        typ = typ 
              |> function
                 | Some t -> t 
                 | None -> Auctions.TimedAscending { // if no typ serialized, use english
                    reservePrice=Amount.zero currency
                    minRaise =Amount.zero currency
                    timeFrame = TimeSpan.FromSeconds(0.0)
                  } 
      }// null ref expn
    
    AddAuction(at, auction)
  | "PlaceBid" -> 
    let id = 
      entries
      |> findEntryStr "Id"
      |> BidId.Parse
    
    let auction = entries |> findEntryInt64 "Auction"
    let amount = entries |> findEntryInt64 "AmountValue"
    let currency = entries |> findEntryStr "AmountCurrency" |> Currency.ofValue
    
    let user = 
      entries
      |> findEntryStr "User"
      |> User.tryParse
    
    let at = 
      entries
      |> findEntryInt64 "At"
      |> DateTime
    
    let bid : Bid = 
      { id = id
        auction = auction
        amount = 
          { value = amount
            currency = currency }
        user = user.Value// null ref expn
        at = at }
    
    PlaceBid(at, bid)
  | v -> failwithf "Unknown type %s" v