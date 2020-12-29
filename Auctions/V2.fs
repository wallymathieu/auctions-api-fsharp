module Auctions.Domain.V2
open FSharpPlus
open Fleece.FSharpData
open Fleece.FSharpData.Operators

type AuctionV2 = Auction of Auction
type AuctionV2 with
  static member JsonObjCodec =
    fun id startsAt title expiry user typ currency -> Auction { id =id; startsAt =startsAt; title = title; expiry=expiry; user=user; typ=typ; currency=currency }
    |> withFields
    |> jfield "id"        (fun (Auction x) -> x.id)
    |> jfield "startsAt"  (fun (Auction x) -> x.startsAt)
    |> jfield "title"     (fun (Auction x) -> x.title)
    |> jfield "expiry"    (fun (Auction x) -> x.expiry)
    |> jfield "user"      (fun (Auction x) -> x.user)
    |> jfield "type"      (fun (Auction x) -> x.typ)
    |> jfield "currency"  (fun (Auction x) -> x.currency)
type BidV2 = Bid of Bid
type BidV2 with
  static member JsonObjCodec =
    fun id auction user amount at-> Bid { id =id; auction =auction; user = user; amount=amount; at=at }
    |> withFields
    |> jfield "id"      (fun (Bid x) -> x.id)
    |> jfield "auction" (fun (Bid x) -> x.auction)
    |> jfield "user"    (fun (Bid x) -> x.user)
    |> jfield "amount"  (fun (Bid x) -> x.amount)
    |> jfield "at"      (fun (Bid x) -> x.at)
type CommandV2 = Command of Command
type CommandV2 with
  static member OfJson json =
    match json with
    | JObject o -> monad {
        let! t = o .@ "$type"
        match t with
        | "AddAuction" ->
          let create d (Auction a) = Command <| AddAuction (d,a)
          return! (create <!> (o .@ "at") <*> (o .@ "auction"))
        | "PlaceBid" ->
          let create d (Bid b) = Command <| PlaceBid (d,b)
          return! (create <!> (o .@ "at") <*> (o .@ "bid"))
        | x -> return! (Decode.Fail.invalidValue json (sprintf "Expected one of: 'AddAuction', 'PlaceBid' for $type but %s" x))
      }
    | x -> Decode.Fail.objExpected json
  static member ToJson (Command x) =
    match x with
    | AddAuction (d,a)-> jobj [ "$type" .= "AddAuction"; "at" .= d; "auction" .= (Auction a)]
    | PlaceBid (d,b)-> jobj [ "$type" .= "PlaceBid"; "at" .= d; "bid" .= (Bid b)]
type EventV2 = Event of Event
type EventV2 with
  static member Unwrap (Event e) = e
  static member OfJson json =
    match json with
    | JObject o -> monad {
        let! t = o .@ "$type"
        match t with
        | "AuctionAdded" ->
          let create d (Auction a) = Event <| AuctionAdded (d,a)
          return! (create <!> (o .@ "at") <*> (o .@ "auction"))
        | "BidAccepted" ->
          let create d (Bid b) = Event <| BidAccepted (d,b)
          return! (create <!> (o .@ "at") <*> (o .@ "bid"))
        | x -> return! (Decode.Fail.invalidValue json (sprintf "Expected one of: 'AuctionAdded', 'BidAccepted' for $type but %s" x))
      }
    | x -> Decode.Fail.objExpected json
  static member ToJson (Event x) =
    match x with
    | AuctionAdded (d,a)-> jobj [ "$type" .= "AuctionAdded"; "at" .= d; "auction" .= Auction a]
    | BidAccepted (d,b)-> jobj [ "$type" .= "BidAccepted"; "at" .= d; "bid" .= Bid b]
type ObservableV2 = Observable of Observable
type ObservableV2 with
  static member ToJson (Observable x) =
    match x with
    | Commands commands->
      jobj [ "$type" .= "Commands"; "commands" .= List.map Command commands]
    | Results results->
      let mapToJson = function | Ok o-> JArray [|JString "Ok"; toJson <| Event o|] | Error e->JArray [JString "Error";toJson e]
      let jresults = results |> List.map mapToJson |> List.toArray |> JArray
      jobj [ "$type" .= "Results"; "results", jresults]

