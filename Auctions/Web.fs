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
open Newtonsoft.Json

(* Fake auth in order to simplify web testing *)

type Session = 
  | NoSession
  | UserLoggedOn of User

let authenticated f = fun (next:HttpFunc) (httpContext:HttpContext) -> 
    (match httpContext.Request.Headers.TryGetValue "x-fake-auth" with
    | (true, u) -> 
        User.tryParse (u.ToString()) 
        |> function | Some user-> f (UserLoggedOn(user)) | None ->f NoSession
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
type AddAuctionReq = {
  id : AuctionId
  startsAt : DateTime
  title : string
  endsAt : DateTime
  currency : string
  [<JsonProperty("type")>]
  typ:string
}
type BidJsonResult = { 
  amount:Amount
  bidder:string
}
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
module JsonResult=
  let exnToInvalidUserData (err:exn)=InvalidUserData err.Message

  let getAuctionResult (auction,bids,maybeAmountAndWinner) =
    let now =DateTime.UtcNow
    let discloseBidders =Auction.biddersAreOpen auction
    let mapBid (b:Bid) :BidJsonResult = { 
      amount=b.amount
      bidder= if discloseBidders 
              then b.user.ToString() 
              else b.user.GetHashCode().ToString() // here you might want bidder number
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
         { user = user; id= BidId.NewGuid();
           amount=a.amount;auction=id
           at = d })
        |> PlaceBid)
      return Result.mapError exnToInvalidUserData res
    }

let webPart (agent : AuctionDelegator) = 

  let overview = 
    GET >=> fun (next:HttpFunc) ctx ->
            task {
              let! r = agent.GetAuctions() |> Async.StartAsTask
              return! (json (r |>List.toArray)) next ctx
            }

  let getAuctionResult id=
    monad {
      let! auctionAndBids = agent.GetAuction id
      match auctionAndBids with
      | Some v-> return Ok <| JsonResult.getAuctionResult v
      | None -> return Error (UnknownAuction id)
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
  let placeBid (id : AuctionId) = 
    authenticated (function 
      | NoSession -> unauthorized "Not logged in"
      | UserLoggedOn user -> 
        POST >=> handleCommandAsync (JsonResult.toPostedPlaceBid id user))
  
  choose [ route "/" >=> (text "")
           route Paths.Auction.overview >=> overview
           route Paths.Auction.register >=> register
           routef Paths.Auction.details details
           routef Paths.Auction.placeBid placeBid ]

//curl  -X POST -d '{ "id":"1","startsAt":"2017-01-01","endsAt":"2018-01-01","title":"First auction" }' -H "x-fake-auth: BuyerOrSeller|a1|Seller"  -H "Content-Type: application/json"  127.0.0.1:8083/auction

//curl  -X POST -d '{ "auction":"1","amount":"VAC10" }' -H "x-fake-auth: BuyerOrSeller|a1|Test"  -H "Content-Type: application/json"  127.0.0.1:8083/auction/1/bid 