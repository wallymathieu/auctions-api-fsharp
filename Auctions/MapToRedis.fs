module MapToRedis
open StackExchange.Redis
open System
open Commands

let redisKey (str:string)= RedisKey.op_Implicit( str )
let redisValueStr (str:string)= RedisValue.op_Implicit str
let redisValueInt64 (v:int64)=RedisValue.op_Implicit v
let redisValueFloat (v:float)=RedisValue.op_Implicit v
let valueToKey (v:RedisValue) : RedisKey=
    RedisKey.op_Implicit( v.ToString())

let hashEntryStr key value=
    new HashEntry(redisValueStr key, redisValueStr value)

let hashEntryInt64 key value=
    new HashEntry(redisValueStr key, redisValueInt64 value)

let hashEntryFloat key value=
    new HashEntry(redisValueStr key, redisValueFloat value)

let mapToHashEntries command=
    let withType t xs=
        hashEntryStr "Type" t :: xs

    match command with
    | Empty(at=at)->
        [
        hashEntryStr "Id" (id.ToString())
        hashEntryInt64 "At" at.Ticks
        ] |> withType "Empty"
    | AddAuction(id=id;title=title;endsAt=endsAt;at=at)->
        [
        hashEntryStr "Id" (id.ToString())
        hashEntryStr "Title" (title)
        hashEntryInt64 "EndsAt" endsAt.Ticks
        hashEntryInt64 "At" at.Ticks
        ] |> withType "AddAuction"
    | PlaceBid(id=id;auction=auction;amount=amount;user=user;at=at)->
        [
        hashEntryStr "Id" (id.ToString())
        hashEntryStr "Auction" (auction.ToString())
        hashEntryFloat "AmountValue" amount.value
        hashEntryStr "AmountCurrency" (amount.currency.ToString())
        hashEntryInt64 "At" at.Ticks
        hashEntryStr "User" (user.ToString())
        ] |> withType "PlaceBid"

    | RemoveBid(id=id;user=user;at=at)->
        [
        hashEntryStr "Id" (id.ToString())
        hashEntryInt64 "At" at.Ticks
        hashEntryStr "User" (user.ToString())
        ] |> withType "RemoveBid"

let findEntry key (entries:HashEntry array) =
    let k = redisValueStr key
    let entry = entries |> Array.find ( fun e-> e.Name.Equals(k) )
    entry

let findEntryStr key entries=
    let entry=findEntry key entries
    entry.Value.ToString()
let findEntryInt64 key entries: int64=
    let entry=findEntry key entries
    match Int64.TryParse( entry.Value.ToString() )with
    | true, v->v
    | false,_->failwithf "unable to parse value %A" entry.Value
let findEntryFloat key entries=
    let entry=findEntry key entries
    match Double.TryParse( entry.Value.ToString() )with
    | true, v->v
    | false,_->failwithf "unable to parse value %A" entry.Value

let mapFromHashEntries entries : Command=
    let t = entries|> findEntryStr "Type"
    match t with
    | "Empty" ->
        let at = entries|>findEntryInt64 "At" |> DateTime
        Empty(at=at)
    | "AddAuction" ->
        let id = entries|>findEntryStr "Id" |> Domain.AuctionId.Parse
        let title = entries|>findEntryStr "Title"
        let endsAt = entries|>findEntryInt64 "EndsAt" |> DateTime
        let at = entries|>findEntryInt64 "At" |> DateTime
        AddAuction(id=id,title=title,endsAt=endsAt,at=at)
    | "PlaceBid" ->
        let id = entries|>findEntryStr "Id" |> Guid.Parse
        let auction = entries|>findEntryStr "Auction" |> Guid.Parse
        let amount = entries|>findEntryFloat "AmountValue" 
        let currency = entries|>findEntryStr "AmountCurrency" 
        let user = entries|>findEntryStr "User" |> Guid.Parse
        let at = entries|>findEntryInt64 "At" |> DateTime
        PlaceBid(id=id,auction=auction,amount={value=amount;currency=currency},user=user,at=at)
    | "RemoveBid" ->
        let id = entries|>findEntryStr "Id" |> Guid.Parse
        let user = entries|>findEntryStr "User" |> Guid.Parse
        let at = entries|>findEntryInt64 "At" |> DateTime
        RemoveBid(id=id,user=user,at=at)
    | v -> failwithf "Unknown type %s" v