module Auctions.Web
open System
open FSharpPlus
open Giraffe
open Microsoft.AspNetCore.Http

open Auctions.Security.Cryptography
open Auctions.Domain
open Auctions.Actors
open Auctions
open FSharpPlus.Data
open System.Text
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open FSharp.Data
open FSharp.Control.Tasks.V2
open Auctions.Giraffe.Json
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
    let create sub name userType= {subject =sub ; name=name; userType =parse userType}
    match json with
    | JObject o -> create <!> (o .@ "sub") <*> (o .@ "name") <*> (o .@ "u_typ")
    | x -> Decode.Fail.objExpected x

let authenticated f = fun (next:HttpFunc) (httpContext:HttpContext) ->
    (match httpContext.Request.Headers.TryGetValue "x-jwt-payload" with
    | (true, u) ->
      string u
      |>Convert.FromBase64String
      |>Encoding.UTF8.GetString
      |> parseJson
      |> Result.bind( fun (payload:JwtPayload)->
        let userId = UserId payload.subject
        match payload.userType with
        | UserType.BuyerOrSeller -> BuyerOrSeller(userId, payload.name) |> Ok
        | UserType.Support -> Support userId |> Ok
        | v -> Decode.Fail.invalidValue (v |> string |> JString) "Unknown user type")
      |> function | Ok user->f (UserLoggedOn(user))
                  | Error _ ->f NoSession
    | _ -> f NoSession) next httpContext


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
module OfJson=
  type Typ = Domain.Auctions.Type
  let bidReq (auctionId, user, at) json =
    let create a = { user = user; id= BidId.New(); amount=a; auction=auctionId; at = at }
    match json with
    | JObject o -> create <!> (o .@ "amount")
    | x -> Decode.Fail.objExpected x
    |> Result.mapError string
  let addAuctionReq (user) json =
    let create id startsAt title endsAt (currency:string option) (typ:string option)
      =
        let currency= currency |> Option.bind Currency.tryParse |> Option.defaultValue Currency.VAC
        let defaultTyp = TimedAscending { reservePrice=Amount.zero currency; minRaise =Amount.zero currency;
          timeFrame =TimeSpan.FromSeconds(0.0) }
        let typ = typ |> Option.bind Typ.TryParse
                      |> Option.defaultValue defaultTyp
        { user = user; id=id; startsAt=startsAt; expiry=endsAt; title=title; currency=currency; typ=typ }
    match json with
    | JObject o -> create <!> (o .@ "id") <*> (o .@ "startsAt") <*> (o .@ "title")<*> (o .@ "endsAt")
                   <*> (o .@? "currency")<*> (o .@? "type")
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

let webApp (agent : AuctionDelegator) =

  let overview = GET >=> fun (next:HttpFunc) ctx -> task {
    let! auctionList =  agent.GetAuctions()
    let jArray = auctionList |> List.map ToJson.auction |> List.toArray |> JArray
    return! json jArray next ctx
  }

  let details id  = GET >=> fun next ctx -> task{
    let id = AuctionId id
    match! agent.GetAuction id with
    | Some auction -> return! json(auction) next ctx
    | None -> return! setStatusCode 404 next ctx
  }
  /// handle command and add result to repository
  let handleCommandAsync
    (maybeC:Result<Command,_>) :Async<Result<_,_>> =
    monad {
      match agent.UserCommand <!> maybeC with
      | Ok asyncR->
          let! result = asyncR
          return result
      | Error c'->return Error c'
    }
  /// turn handle command to webpart
  let handleCommandAsync toCommand =
    fun next ctx ->
      task {
        let! maybeCommand = toCommand ctx
        let! commandResult= handleCommandAsync maybeCommand
        return! (Json.OK_or_BAD_REQUEST commandResult) next ctx
      }
  let unauthorized txt= setStatusCode 401 >=> text txt
  /// register auction
  let register =
    let toPostedAuction user ctx= task {
        let! body = Json.getBody ctx
        return body
               |> Result.bind (OfJson.addAuctionReq (user))
               |> Result.map (Timed.atNow >>AddAuction)
               |> Result.mapError InvalidUserData
    }
    authenticated (function
      | NoSession -> unauthorized "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (toPostedAuction user))

  /// place bid
  let placeBid id =
    let auctionId = AuctionId id
    let toPostedPlaceBid id user ctx=task{
      let! body = Json.getBody ctx
      return body
             |> Result.bind (OfJson.bidReq (id,user,DateTime.UtcNow) )
             |> Result.map (Timed.atNow >>PlaceBid)
             |> Result.mapError InvalidUserData
    }
    authenticated (function
      | NoSession -> unauthorized "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (toPostedPlaceBid auctionId user)
      )

  choose [ route "/" >=> (text "")
           route Paths.Auction.overview >=> overview
           route Paths.Auction.register >=> register
           routef Paths.Auction.details details
           routef Paths.Auction.placeBid placeBid ]

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
