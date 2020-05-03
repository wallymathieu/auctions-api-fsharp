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
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open System.Security.Cryptography.X509Certificates
open System.Runtime.InteropServices

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
module JwtPayload=
  let decode (payload:JwtPayload)=
    let userId = UserId payload.subject
    match payload.userType with
    | UserType.BuyerOrSeller -> BuyerOrSeller(userId, payload.name) |> Ok
    | UserType.Support -> Support userId |> Ok
    | v -> Decode.Fail.invalidValue (v |> string |> JString) "Unknown user type"
let context apply (a : Suave.Http.HttpContext) = apply a a
let (|Bearer|_|) (s:string) =
  let bearer = "Bearer " in if s.StartsWith bearer then Some <| s.Substring(bearer.Length) else None
let authenticatedWithJwt (file:string, key:string, issuers:string list) f = //
  let storageFlags=
    // https://github.com/dotnet/aspnetcore/blob/master/src/Identity/ApiAuthorization.IdentityServer/src/Configuration/ConfigureSigningCredentials.cs
    let unsafeEphemeralKeySet =  enum<X509KeyStorageFlags> 32
    if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
      unsafeEphemeralKeySet
    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
      X509KeyStorageFlags.PersistKeySet
    else
      X509KeyStorageFlags.DefaultKeySet
  let cert = new X509Certificate2(file, key, storageFlags)
  let credentials= SigningCredentials(X509SecurityKey(cert), "RS256")
  let vp = TokenValidationParameters()
  vp.IssuerSigningKeys <- [credentials.Key]
  vp.ValidIssuers <- issuers
  let jwth = JwtSecurityTokenHandler()
  let (|ValidToken|_|) (token:string)=
    try
      if jwth.CanReadToken token then
        let t =ref Unchecked.defaultof<SecurityToken>
        jwth.ValidateToken(token, vp, t) |> ignore
        let jwtToken = t.Value :?> JwtSecurityToken
        Some jwtToken
      else None
    with | _ -> None // decoding failed
  context (fun x ->
    match x.request.header "Authorization" with
    | Choice1Of2 u ->
      match u with
      | Bearer (ValidToken jwtToken)->
        jwtToken.RawData
        |> parseJson
        |> Result.bind JwtPayload.decode
        |> function | Ok user->f (UserLoggedOn(user))
                    | Error _ ->f NoSession
      | _ ->f NoSession
    | Choice2Of2 _ -> f NoSession)

/// Assuming front proxy verification of auth in order to simplify web testing
let proxyAuthenticated f = //
  context (fun x ->
    match x.request.header "x-jwt-payload" with
    | Choice1Of2 u ->
      Convert.FromBase64String u
      |>Encoding.UTF8.GetString
      |> parseJson
      |> Result.bind JwtPayload.decode
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
module OfJson=
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
        let typ = typ |> Option.bind Type.tryParse
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

let webPart authenticated (agent : AuctionDelegator) =

  let overview : WebPart= GET >=> fun (ctx) -> monad {
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
