module Auctions.Web
open System
open FSharpPlus
open Auctions.Suave
open Auctions.Suave.Writers
open Auctions.Suave.Filters
open Auctions.Suave.Successful
open Auctions.Suave.RequestErrors
open Auctions.Security.Cryptography

open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators
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
  | BuyerOrSeller=0
  | Support=1
type JwtPayload = { subject:string; name:string; userType:UserType }
with
  static member OfJson json:ParseResult<JwtPayload> =
    let create sub name userType= {subject =sub ; name=name; userType =parse userType}
    match json with
    | JObject o -> create <!> (o .@ "sub") <*> (o .@ "name") <*> (o .@ "u_typ")
    | x -> Decode.Fail.objExpected x
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
        | v -> Decode.Fail.invalidValue (v |> string |> JString) "Unknown user type")
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

type IJsonConverter=
  abstract member OfJsonBid: auctionId:AuctionId * user:User * at:DateTime -> json:JsonValue -> Result<Bid,string>
  abstract member OfJsonAuction: user:User -> json:JsonValue -> Result<Auction,string>
  abstract member ToJsonAuction: auction:Auction -> JsonValue
  abstract member ToJsonAuctionAndBidsAndMaybeWinnerAndAmount: auction:Auction * Bid list * ('a * 'b) option -> JsonValue
  //Auction * Bid list * ('a * 'b) option -> JsonValue

(* Json API *)
let V1Json = { new IJsonConverter with
               member __.OfJsonBid (auctionId, user, at) json = V1.OfJson.bidReq (auctionId, user, at) json
               member __.OfJsonAuction s json = V1.OfJson.addAuctionReq s json
               member __.ToJsonAuction a = V1.ToJson.auction a
               member __.ToJsonAuctionAndBidsAndMaybeWinnerAndAmount (auction, bids, maybeAmountAndWinner) =
                 V1.ToJson.auctionAndBidsAndMaybeWinnerAndAmount (auction, bids, maybeAmountAndWinner)
             }
let V2Json = { new IJsonConverter with
               member __.OfJsonBid (auctionId, user, at) json = V2.OfJson.bidReq (auctionId, user, at) json
               member __.OfJsonAuction s json = V2.OfJson.addAuctionReq s json
               member __.ToJsonAuction a = V2.ToJson.auction a
               member __.ToJsonAuctionAndBidsAndMaybeWinnerAndAmount (auction, bids, maybeAmountAndWinner) =
                 V2.ToJson.auctionAndBidsAndMaybeWinnerAndAmount (auction, bids, maybeAmountAndWinner)
             }
let versioned f = fun ctx ->
  let version (ctx : Suave.Http.HttpContext) = //
    match ctx.request.header "x-version" with
    | Choice1Of2 "1" -> Some V1Json
    | Choice1Of2 "2" -> Some V2Json
    | Choice2Of2 _   -> Some V2Json
    | _              -> None

  match version ctx with
  | Some jsonC -> f jsonC ctx
  | None -> BAD_REQUEST "Invalid version" ctx

let webPart (agent : AuctionDelegator) =
  let overview : WebPart= GET >=> versioned (fun jsonC ctx -> monad {
    let! auctionList =  agent.GetAuctions() |> liftM Some |> OptionT
    let json = auctionList |> List.map jsonC.ToJsonAuction |> List.toArray |> JArray
    return! Json.OK json ctx
  })

  let details id : WebPart= GET >=> versioned (fun jsonC ctx -> monad{
    let id = AuctionId id
    let! auctionAndBids = lift (agent.GetAuction id)
    return!
       match auctionAndBids with
       | Some v -> Json.OK (jsonC.ToJsonAuctionAndBidsAndMaybeWinnerAndAmount v) ctx
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
            return! Json.OK (toJson commandSuccess) ctx
          | Error e -> return! Json.BAD_REQUEST (toJson e) ctx
      | Error c' -> return! Json.BAD_REQUEST (toJson c') ctx
    }

  /// register auction
  let register =
    let toPostedAuction user =
        Json.getBody
          >> Result.bind (OfJson.addAuctionReq (user))
          >> Result.map (Timed.atNow >>AddAuction)
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
        >> Result.bind (OfJson.bidReq (id,user,DateTime.UtcNow) )
        >> Result.map (Timed.atNow >>PlaceBid)
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
