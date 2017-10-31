module Auctions.Web
open System
open Suave
open Suave.Filters
open Suave.Operators
open Suave.RequestErrors
open Suave.Successful
open Suave.Writers

open Auctions.Domain
open Auctions.Actors
open Auctions
open Auctions.Commands
open Newtonsoft.Json

(* Fake auth in order to simplify web testing *)

type Session = 
  | NoSession
  | UserLoggedOn of User

let authenticated f = 
  context (fun x -> 
    match x.request.header "x-fake-auth" with
    | Choice1Of2 u -> 
        User.tryParse u |> function | Some user->
                                        f (UserLoggedOn(user))
                                    | None ->f NoSession
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
  endsAt : DateTime
  currency : Currency
  bids : BidJsonResult array
  winner : string
  winnerPrice : string
}
module MapToWeb=
  let exnToInvalidUserData (err:exn)=InvalidUserData err.Message

  let getAuctionResult (a:Auction,bidsOrEnded:Choice<Bid list,(Amount*User) option>) =
    let now =DateTime.UtcNow
    let discloseBidders =Auction.biddersAreOpen a
    let mapBid (b:Bid) :BidJsonResult = { 
      amount=b.amount
      bidder= if discloseBidders 
              then b.user.ToString() 
              else b.user.GetHashCode().ToString() // here you might want bidder number
    }
    let bids =match bidsOrEnded with
              | Choice1Of2 bids->bids
              | Choice2Of2 _->[] 

    let bids = match a.typ with
               | English _ -> bids
               // the bids are not disclosed until after the end :
               | Vickrey -> if Auction.hasEnded now a then bids else []
               | Blind -> if Auction.hasEnded now a then bids else [] 
    let (winner,winnerPrice) = 
              match bidsOrEnded with
              | Choice2Of2 None
              | Choice1Of2 _ -> ("", "") // Either no winner or the auction has not ended yet
              | Choice2Of2 (Some (amount, winner))->
                  (winner.ToString(), amount.ToString())
    { id=a.id; startsAt=a.startsAt; title=a.title;endsAt=a.endsAt; currency=a.currency
      bids=bids |> List.map mapBid |> List.toArray
      winner = winner
      winnerPrice = winnerPrice
    }

  let toPostedAuction user = 
      getBodyAsJSON<AddAuctionReq> 
        >> Result.map (fun a -> 
        let currency= Currency.tryParse a.currency |> Option.getOrElse Currency.VAC
        
        let typ = Type.tryParse a.typ |> Option.getOrElse (English { // default is english auctions
                                                                      reservePrice=Amount.zero currency 
                                                                      minRaise =Amount.zero currency
                                                                    })
        { user = user
          id=a.id
          startsAt=a.startsAt
          endsAt=a.endsAt
          title=a.title
          currency=currency
          typ=typ
        }
        |> Timed.atNow
        |> Commands.AddAuction)
        >> Result.mapError exnToInvalidUserData

  let toPostedPlaceBid id user = 
    getBodyAsJSON<BidReq> 
      >> Result.map (fun a -> 
      let d = DateTime.UtcNow
      (d, 
       { user = user; id= BidId.NewGuid();
         amount=a.amount;auction=id
         at = d })
      |> Commands.PlaceBid)
      >> Result.mapError exnToInvalidUserData

let webPart (agent : AuctionDelegator) = 

  let overview = 
    GET >=> fun (ctx) ->
            async {
              let! r = agent.GetAuctions()
              return! JSON (r |>List.toArray) ctx
            }

  let getAuctionResult id=
    asyncResult{
      let! (a,bidsOrEnded) = async{
                        let! auctionAndBids = agent.GetAuction id
                        return match auctionAndBids with
                                | Some v-> Ok v
                                | None -> Error (UnknownAuction id)
                      }
      return MapToWeb.getAuctionResult (a,bidsOrEnded)
    }

  let details id  = GET >=> JSON(getAuctionResult id)

  /// handle command and add result to repository
  let handleCommandAsync (maybeC:Result<_,_>) = 
    asyncResult{
      let! c=maybeC
      return agent.UserCommand c
    }
  /// turn handle command to webpart
  let handleCommandAsync toCommand: WebPart = 
    fun (ctx : HttpContext) ->
      async {
        let r = toCommand ctx
        let! commandResult= handleCommandAsync r
        return! JSONorBAD_REQUEST commandResult ctx
      }

  /// register auction
  let register = 
    authenticated (function 
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user -> 
        POST >=> handleCommandAsync (MapToWeb.toPostedAuction user))

  /// place bid
  let placeBid (id : AuctionId) = 
    authenticated (function 
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user -> 
        POST >=> handleCommandAsync (MapToWeb.toPostedPlaceBid id user))
  
  choose [ path "/" >=> (Successful.OK "")
           path Paths.Auction.overview >=> overview
           path Paths.Auction.register >=> register
           pathScan Paths.Auction.details details
           pathScan Paths.Auction.placeBid placeBid ]

//curl  -X POST -d '{ "id":"1","startsAt":"2017-01-01","endsAt":"2018-01-01","title":"First auction" }' -H "x-fake-auth: BuyerOrSeller|a1|Seller"  -H "Content-Type: application/json"  127.0.0.1:8083/auction

//curl  -X POST -d '{ "auction":"1","amount":"VAC10" }' -H "x-fake-auth: BuyerOrSeller|a1|Test"  -H "Content-Type: application/json"  127.0.0.1:8083/auction/1/bid 