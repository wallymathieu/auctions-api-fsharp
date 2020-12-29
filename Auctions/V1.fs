module Auctions.V1
open System
open FSharpPlus
open Auctions.Security.Cryptography
open Auctions.Domain
open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open FSharp.Data
let auctionId = "id"
let startsAt = "startsAt"
let title = "title"
let expiry = "expiry"
let currency = "currency"

module OfJson=
  type Typ = Domain.Auctions.Type
  let bidReq (auctionId, user, at) json =
    let create a = { user = user; id= BidId.New(); amount=a; auction=auctionId; at = at }
    match json with
    | JObject o -> create <!> (o .@ "amount")
    | x -> Decode.Fail.objExpected x
    |> Result.mapError string
  let addAuctionReq user json =
    let create id startsAt title endsAt (currency:string option) (typ:string option)
      =
        let currency= currency |> Option.bind Currency.tryParse |> Option.defaultValue Currency.VAC
        let defaultTyp = TimedAscending { reservePrice=Amount.zero currency; minRaise =Amount.zero currency;
          timeFrame =TimeSpan.FromSeconds(0.0) }
        let typ = typ |> Option.bind Typ.TryParse
                      |> Option.defaultValue defaultTyp
        { user = user; id=id; startsAt=startsAt; expiry=endsAt; title=title; currency=currency; typ=typ }
    match json with
    | JObject o -> create <!> (o .@ auctionId) <*> (o .@ startsAt) <*> (o .@ title)<*> (o .@ expiry)
                   <*> (o .@? currency)<*> (o .@? "type")
    | x -> Decode.Fail.objExpected x
    |> Result.mapError string

module ToJson=
  let auctionList (auction:Auction) =[
      auctionId .= auction.id
      startsAt .= auction.startsAt
      title .= auction.title
      expiry .= auction.expiry
      currency .= auction.currency
    ]
  let auction auction = auctionList auction |> jobj
  let auctionAndBidsAndMaybeWinnerAndAmount (auction, bids, maybeAmountAndWinner) : JsonValue =
    let (winner,winnerPrice) =
            match maybeAmountAndWinner with
            | None -> ("","")
            | Some (amount, winner)->
                (winner.ToString(), amount.ToString())
    let discloseBidders =Auction.biddersAreOpen auction
    let bid (x: Bid) :JsonValue =
      let userId = string x.user
      jobj [
        "amount" .= x.amount
        "bidder" .= (if discloseBidders then userId else SHA512.ofString userId)
      ]
    let bids = (List.map bid bids |> List.toArray |> JArray)
    auctionList auction @ [
      "bids", bids
      "winner" .= winner
      "winnerPrice" .= winnerPrice
    ] |> jobj
