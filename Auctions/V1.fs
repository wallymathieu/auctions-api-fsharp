module Auctions.Domain.V1
open FSharpPlus
open Fleece.FSharpData
open Fleece.FSharpData.Operators

type AuctionV1 = Auction of Auction
type AuctionV1 with
  static member JsonObjCodec =
    fun id startsAt title expiry user typ currency -> Auction { id =id; startsAt =startsAt; title = title; expiry=expiry; user=user; typ=typ; currency=currency }
    |> withFields
    |> jfield "auctionId"          (fun (Auction x) -> x.id)
    |> jfield "auctionStarts"      (fun (Auction x) -> x.startsAt)
    |> jfield "auctionDescription" (fun (Auction x) -> x.title)
    |> jfield "auctionExpires"     (fun (Auction x) -> x.expiry)
    |> jfield "whoCreatedMe"       (fun (Auction x) -> x.user)
    |> jfield "kindOfAuction"      (fun (Auction x) -> x.typ)
    |> jfield "currency"           (fun (Auction x) -> x.currency)
type BidV1 = Bid of Bid
type BidV1 with
  static member JsonObjCodec =
    fun id auction user amount at-> Bid { id =id; auction =auction; user = user; amount=amount; at=at }
    |> withFields
    |> jfield "bidId"         (fun (Bid x) -> x.id)
    |> jfield "auctionId"     (fun (Bid x) -> x.auction)
    |> jfield "whyCreatedMe"  (fun (Bid x) -> x.user)
    |> jfield "bidAmount"     (fun (Bid x) -> x.amount)
    |> jfield "placedAt"      (fun (Bid x) -> x.at)
type CommandV1 = Command of Command
type CommandV1 with
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
type EventV1 = Event of Event
type EventV1 with
  static member OfJson json =
    match json with
    | JObject o -> monad {
        let! t = o .@ "$type"
        match t with
        | "@AuctionAdded" ->
          let create d (Auction a) = Event <| AuctionAdded (d,a)
          return! (create <!> (o .@ "at") <*> (o .@ "Auction"))
        | "@BidAccepted" ->
          let create d (Bid b) = Event <| BidAccepted (d,b)
          return! (create <!> (o .@ "at") <*> (o .@ "Bid"))
        | x -> return! (Decode.Fail.invalidValue json (sprintf "Expected one of: '@AuctionAdded', '@BidAccepted' for $type but %s" x))
      }
    | x -> Decode.Fail.objExpected json
  static member ToJson (Event x) =
    match x with
    | AuctionAdded (d,a)-> jobj [ "$type" .= "@AuctionAdded"; "at" .= d; "Auction" .= Auction a]
    | BidAccepted (d,b)-> jobj [ "$type" .= "@BidAccepted"; "at" .= d; "Bid" .= Bid b]
type ObservableV1 = Observable of Observable
type ObservableV1 with
  static member ToJson (Observable x) =
    match x with
    | Commands commands->
      jobj [ "$type" .= "@ListOfCommands"; "commands" .= List.map Command commands]
    | Results results->
      let mapToJson = function | Ok o-> JArray [|JString "Ok"; toJson <| Event o|] | Error e->JArray [JString "Error";toJson e]
      let jresults = results |> List.map mapToJson |> List.toArray |> JArray
      jobj [ "$type" .= "@ListOfResults"; "results", jresults]

