module Auctions.Web
open System
open FSharpPlus
open Giraffe

open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http

open Auctions.Domain
open Auctions.Actors
open Auctions
open FSharpPlus.Data
open System.Text
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open FSharp.Data
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
        | _ -> Error "Unknown user type")
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
  let exnToInvalidUserData (err:exn)=InvalidUserData err.Message

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

  let toPostedAuction user httpContext =
    task {
       let! body = getBodyAsJSON<AddAuctionReq> httpContext

       let res = body |> Result.map (fun a ->
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
      return Result.mapError exnToInvalidUserData res
    }

  let toPostedPlaceBid id user httpContext=
    task {
      let! body = getBodyAsJSON<BidReq> httpContext
      let res = body |> Result.map (fun a ->
        let d = DateTime.UtcNow
        (d,
         { user = user; id=Guid.NewGuid() |> BidId
           amount=a.amount;auction=id
           at = d })
        |> PlaceBid)
      return Result.mapError exnToInvalidUserData res
    }

let webApp (agent : AuctionDelegator) =

  let overview =
    GET >=> fun (next:HttpFunc) ctx ->
            task {
              let! r = agent.GetAuctions() |> Async.StartAsTask
              return! (json (r |>List.toArray)) next ctx
            }

  let getAuctionResult id=
    let auctionId = AuctionId id
    monad {
      let! auctionAndBids = agent.GetAuction auctionId
      match auctionAndBids with
      | Some v-> return Ok <| JsonResult.getAuctionResult v
      | None -> return Error (UnknownAuction auctionId)
    }

  let details id  = GET >=> json(getAuctionResult id)

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
      monad {
        let! r = toCommand ctx
        let! commandResult= handleCommandAsync r |> Async.StartAsTask
        return! (JSONorBAD_REQUEST commandResult) next ctx
      }
  let unauthorized txt= setStatusCode 401 >=> text txt
  /// register auction
  let register =
    authenticated (function
      | NoSession -> unauthorized "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (JsonResult.toPostedAuction user))

  /// place bid
  let placeBid id =
    let auctionId = AuctionId id
    authenticated (function
      | NoSession -> unauthorized "Not logged in"
      | UserLoggedOn user ->
        POST >=> handleCommandAsync (JsonResult.toPostedPlaceBid auctionId user))

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
