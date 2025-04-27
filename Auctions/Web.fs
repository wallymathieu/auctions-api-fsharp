module Auctions.Web
open System
open FSharpPlus
open Auctions.Suave
open Auctions.Suave.Filters
open Auctions.Suave.Successful
open Auctions.Suave.RequestErrors
open Auctions.Security.Cryptography

open Fleece
open Fleece.FSharpData
open FSharp.Data

open Auctions.Domain
open Auctions.Actors
open Auctions
open FSharpPlus.Data
open System.Text
(* Assuming front proxy verification of auth in order to simplify web testing *)

type Session =
  | NoSession
  | UserLoggedOn of User
type UserType=
  | BuyerOrSeller = 0
  | Support = 1
type JwtPayload = { user:User }
with
  static member getUser (payload:JwtPayload)= payload.user
  static member tryCreate (subject: string) (name: string option) (userType: String) =
    let userId = UserId subject
    match tryParse userType, name with
    | Some UserType.BuyerOrSeller, Some name -> { user = BuyerOrSeller(userId, name) } |> Some
    | Some UserType.Support,_ -> { user = Support userId } |> Some
    | _ -> None

  static member OfJson json:ParseResult<JwtPayload> =
    match json with
    | JObject o ->
        JwtPayload.tryCreate <!> (o .@ "sub") <*> (o .@? "name") <*> (o .@ "u_typ")
        >>= (function | Some user-> Ok user | None-> Decode.Fail.invalidValue json "could not interpret as user")
    | x -> Decode.Fail.objExpected x
let decodeXJwtPayloadHeader (headerValue:Choice<String,String>) : Result<JwtPayload,_> =
    match headerValue with
    | Choice1Of2 u ->
      Convert.FromBase64String u
      |>Encoding.UTF8.GetString
      |> ofJsonText
      |> Result.mapError Choice1Of2
    | Choice2Of2 _ -> Error <| Choice2Of2 "Missing value"

let authenticated f = //
  let context apply (a : Suave.Http.HttpContext) = apply a a
  context (fun (x : Suave.Http.HttpContext) ->
      decodeXJwtPayloadHeader (x.request.header "x-jwt-payload")
      |> Result.map JwtPayload.getUser
      |> function | Ok user->f (UserLoggedOn(user))
                  | Error _ ->f NoSession)

module Paths =
  type Int64Path = PrintfFormat<int64 -> string, unit, string, string, int64>

  module Auction =
    /// /auctions
    let overview = "/auctions"
    /// /auctions
    let register = "/auctions"
    /// /auctions/INT
    let details : Int64Path = "/auctions/%d"
    /// /auctions/INT/bid
    let placeBid : Int64Path = "/auctions/%d/bid"

(* Json API *)
module OfJson=
  type Typ = Type
  let bidReq (auctionId, user, at) (json:JsonValue) =
    let create a = { user = user; amount= a ; auction=auctionId; at = at }
    match FSharpData.Encoding json with
    | JObject o -> create <!> (o .@ "amount")
    | x -> Decode.Fail.objExpected x
    |> Result.mapError string
  let addAuctionReq user (json:JsonValue) =
    let create id startsAt title endsAt (currency:string option) (typ:string option) (``open``:bool option)
      =
        let currency= currency |> Option.bind Currency.tryParse |> Option.defaultValue Currency.VAC
        let defaultTyp = TimedAscending { reservePrice = AmountValue.zero; minRaise = AmountValue.zero;
          timeFrame =TimeSpan.FromSeconds(0.0) }
        let typ = typ |> Option.bind Typ.TryParse
                      |> Option.defaultValue defaultTyp
        { user = user; id=id; startsAt=startsAt; expiry=endsAt; title=title; currency=currency; typ=typ
          openBidders = Option.defaultValue false ``open`` }
    match FSharpData.Encoding json with
    | JObject o -> create <!> (o .@ "id") <*> (o .@ "startsAt") <*> (o .@ "title")<*> (o .@ "endsAt")
                   <*> (o .@? "currency")<*> (o .@? "type")<*> (o .@? "open")
    | x -> Decode.Fail.objExpected x
    |> Result.mapError string

module ToJson=
  let auctionList (auction:Auction) =[
      "id" .= auction.id
      "startsAt" .= auction.startsAt
      "title" .= auction.title
      "expiry" .= auction.expiry
      "currency" .= auction.currency
    ]
  let auction auction = auctionList auction |> jobj
  let auctionAndBidsAndMaybeWinnerAndAmount (auction, bids, maybeAmountAndWinner) =
    let winner,winnerPrice =
            match maybeAmountAndWinner with
            | None -> ("","")
            | Some (amount, winner)->
                (winner.ToString(), amount.ToString())
    let discloseBidders =Auction.biddersAreOpen auction
    let bid (x: Bid) =
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

let webPart (agent : AuctionDelegator) (time:unit->DateTime) =

  let overview : WebPart= GET >=> fun ctx -> monad {
    let! auctionList =  agent.GetAuctions() |> liftM Some |> OptionT
    let json = auctionList |> List.map ToJson.auction |> List.toArray |> JArray
    return! Json.OK json ctx
  }

  let details id : WebPart= GET >=> fun ctx->monad{
    let id = AuctionId id
    let! auctionAndBids = lift (agent.GetAuction id)
    return!
       match auctionAndBids with
       | Some v-> Json.OK (ToJson.auctionAndBidsAndMaybeWinnerAndAmount v) ctx
       | None -> NOT_FOUND (sprintf "Could not find auction with id %O" id) ctx
  }

  /// handle command and add result to repository
  let handleCommandAsync
    (tryGetCommand:_->Result<Command,_>) :WebPart =
    fun ctx -> monad {
      let command = tryGetCommand ctx
      match agent.UserCommand <!> command with
      | Ok asyncResult->
          match! lift asyncResult with
          | Ok commandSuccess->
            return! Json.OK (toJson commandSuccess) ctx
          | Error e-> return! Json.BAD_REQUEST (toJson e) ctx
      | Error c'->return! Json.BAD_REQUEST (toJson c') ctx
    }

  /// register auction
  let register =
    let toPostedAuction user =
        Json.getBody
          >> Result.bind (OfJson.addAuctionReq user)
          >> Result.map (Timed.at (time()) >>AddAuction)
          >> Result.mapError InvalidUserData

    authenticated (function
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (toPostedAuction user)
      )

  /// place bid
  let placeBid auctionId =
    let toPostedPlaceBid id user =
      Json.getBody
        >> Result.bind (OfJson.bidReq (id, user, time()) )
        >> Result.map (Timed.at (time()) >>PlaceBid)
        >> Result.mapError InvalidUserData

    authenticated (function
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (toPostedPlaceBid (AuctionId auctionId) user)
      )

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
