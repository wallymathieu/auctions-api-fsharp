﻿module Auctions.Web
open System
open FSharpPlus
open Auctions.Suave
open Auctions.Suave.Writers
open Auctions.Suave.Filters
open Auctions.Suave.Successful
open Auctions.Suave.RequestErrors

open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open FSharp.Data

open Auctions.Domain
open Auctions.Actors
open Auctions
open FSharpPlus.Data
open System.Text
open Hopac

type AgentAdapter (agent : Job<AuctionDelegator>)=
  member __.GetAuction id =  Job.toAsync <| job{
    let! a = agent
    return! a.GetAuction id
  }
  member __.GetAuctions () =  Job.toAsync <| job{
    let! a = agent
    return! a.GetAuctions()
  }
  member __.UserCommand c =  Job.toAsync <| job{
    let! a = agent
    return! a.UserCommand c
  }

(* Assuming front proxy verification of auth in order to simplify web testing *)

type Session =
  | NoSession
  | UserLoggedOn of User
type UserType=
  | BuyerOrSeller=0
  | Support=1
type JwtPayload = { subject:string; name:string; userType:UserType }
with
  static member OfJson json:ParseResult<JwtPayload> =
    let create sub name userType= {subject =sub ; name=name; userType=Enum.Parse userType}
    match json with
    | JObject o -> create <!> (o .@ "sub") <*> (o .@ "name") <*> (o .@ "u_typ")
    | x -> Error (sprintf "Expected JwtPayload, found %A" x)
let authenticated f = //
  let context apply (a : Suave.Http.HttpContext) = apply a a
  context (fun x ->
    match x.request.header "x-jwt-payload" with
    | Choice1Of2 u ->
      Convert.FromBase64String u
      |>Encoding.UTF8.GetString
      |> parseJson
      |> Result.bind( fun (payload:JwtPayload)->
        let userId = UserId payload.subject
        match payload.userType with
        | UserType.BuyerOrSeller -> BuyerOrSeller(userId, payload.name) |> Ok
        | UserType.Support -> Support userId |> Ok
        | _ -> Error "Unknown user type")
      |> function | Ok user->f (UserLoggedOn(user))
                  | Error _ ->f NoSession
    | Choice2Of2 _ -> f NoSession)

module Paths =
  type Int64Path = PrintfFormat<int64 -> string, unit, string, string, int64>

  module Auction =
    /// /auctions
    let overview = "/auctions"
    /// /auction
    let register = "/auction"
    /// /auction/INT
    let details : Int64Path = "/auction/%d"
    /// /auction/INT/bid
    let placeBid : Int64Path = "/auction/%d/bid"

(* Json API *)

type BidReq = {amount : Amount}
with
  static member OfJson json:ParseResult<BidReq> =
    let create a = {amount =a}
    match json with
    | JObject o -> create <!> (o .@ "amount")
    | x -> Error (sprintf "Expected bid, found %A" x)

type AddAuctionReq = {
  id : AuctionId
  startsAt : DateTime
  title : string
  endsAt : DateTime
  currency : string
  typ:string
}
with
  static member OfJson json:ParseResult<AddAuctionReq> =
    let create id startsAt title endsAt (currency:string option) (typ:string option)= {id =id;startsAt=startsAt;title=title; endsAt=endsAt;currency=Option.defaultValue "" currency; typ=Option.defaultValue "" typ}
    match json with
    | JObject o -> create <!> (o .@ "id") <*> (o .@ "startsAt") <*> (o .@ "title")<*> (o .@ "endsAt")<*> (o .@? "currency")<*> (o .@? "type")
    | x -> Error (sprintf "Expected bid, found %A" x)

type BidJsonResult = {
  amount:Amount
  bidder:string
}
with
  static member ToJson (x: BidJsonResult) :JsonValue =
    jobj [
      "amount" .= x.amount
      "bidder" .= x.bidder
    ]
type AuctionJsonResult = {
  id : AuctionId
  startsAt : DateTime
  title : string
  expiry : DateTime
  currency : Currency
  bids : BidJsonResult array
  winner : string
  winnerPrice : string
}
 with
  static member ToJson (x: AuctionJsonResult) :JsonValue =
    jobj [
      "id" .= x.id
      "startsAt" .= x.startsAt
      "title" .= x.title
      "expiry" .= x.expiry
      "currency" .= x.currency
      "bids" .= x.bids
      "winner" .= x.winner
      "winnerPrice" .= x.winnerPrice
    ]

module JsonResult=
  let getAuctionResult (auction,bids,maybeAmountAndWinner) =
    let discloseBidders =Auction.biddersAreOpen auction
    let mapBid (b:Bid) :BidJsonResult = {
      amount=b.amount
      bidder= if discloseBidders
              then string b.user
              else b.user.GetHashCode() |> string // here you might want bidder number
    }

    let (winner,winnerPrice) =
              match maybeAmountAndWinner with
              | None -> ("","")
              | Some (amount, winner)->
                  (winner.ToString(), amount.ToString())
    { id=auction.id; startsAt=auction.startsAt; title=auction.title;expiry=auction.expiry; currency=auction.currency
      bids=bids |> List.map mapBid |> List.toArray
      winner = winner
      winnerPrice = winnerPrice
    }

  let toPostedAuction user =
      Json.getBody
        >> Result.map (fun (a:AddAuctionReq) ->
        let currency= Currency.tryParse a.currency |> Option.defaultValue Currency.VAC

        let typ = Type.tryParse a.typ |> Option.defaultValue (TimedAscending {
                                                                            reservePrice=Amount.zero currency
                                                                            minRaise =Amount.zero currency
                                                                            timeFrame =TimeSpan.FromSeconds(0.0)
                                                                          })
        { user = user
          id=a.id
          startsAt=a.startsAt
          expiry=a.endsAt
          title=a.title
          currency=currency
          typ=typ
        }
        |> Timed.atNow
        |> AddAuction)
        >> Result.mapError InvalidUserData


  let toPostedPlaceBid id user =
    Json.getBody
      >> Result.map (fun (a:BidReq) ->
      let d = DateTime.UtcNow
      (d,
       { user = user; id= BidId.New();
         amount=a.amount;auction=id
         at = d })
      |> PlaceBid)
      >> Result.mapError InvalidUserData

let webPart (agent : Job<AuctionDelegator>) =
  let agent = AgentAdapter(agent)
  let overview =
    GET >=> fun (ctx) ->
            monad {
              let! auctionList =lift ( agent.GetAuctions())
              return! Json.OK auctionList ctx
            }

  let getAuctionResult id : Async<Result<AuctionJsonResult,Errors>>=
      let id = AuctionId id
      monad {
        let! auctionAndBids = agent.GetAuction id
        match auctionAndBids with
        | Some v-> return Ok (JsonResult.getAuctionResult v)
        | None -> return Error (UnknownAuction id)
      }

  let details id : WebPart= GET >=> (fun ctx->monad{
      let id = AuctionId id
      let! auctionAndBids = lift (agent.GetAuction id)
      return!
         match auctionAndBids with
         | Some v-> Json.OK (JsonResult.getAuctionResult v) ctx
         | None -> NOT_FOUND (sprintf "Could not find auction with id %O" id) ctx
  })

  /// handle command and add result to repository
  let handleCommandAsync
    (tryGetCommand:_->Result<Command,_>) :WebPart =
    fun ctx -> monad {
      let command = tryGetCommand ctx
      match agent.UserCommand <!> command with
      | Ok asyncResult->
          match! lift asyncResult with
          | Ok commandSuccess->
            return! Json.OK commandSuccess ctx
          | Error e-> return! Json.BAD_REQUEST e ctx
      | Error c'->return! Json.BAD_REQUEST c' ctx
    }

  /// register auction
  let register =
    authenticated (function
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (JsonResult.toPostedAuction user)
      )

  /// place bid
  let placeBid id =
    let id = AuctionId id
    authenticated (function
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (JsonResult.toPostedPlaceBid id user))

  WebPart.choose [ path "/" >=> (OK "")
                   path Paths.Auction.overview >=> overview
                   path Paths.Auction.register >=> register
                   pathScan Paths.Auction.details details
                   pathScan Paths.Auction.placeBid placeBid ]

module WebHook=
  open FSharp.Data.HttpRequestHeaders
  open FSharp.Data.HttpContentTypes
  let isError (code:int) = code >=300 || code<200
  /// Send payload to webhook
  let inline ofUri (uri:Uri) payload=async{
    let! res = Http.AsyncRequest(
                string uri,
                headers = [ Accept Json ; ContentType Json],
                body = (toJson payload |> string |>  TextRequest))
    if (isError res.StatusCode ) then failwithf "Status code: %d, response: %O" res.StatusCode res.Body
  }
