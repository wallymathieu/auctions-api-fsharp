[<AutoOpen>]
module Tests.TestsModule
open Auctions.Domain
open System
open FSharpPlus

let auctionId = AuctionId 1L
let title = "auction"
let startsAt = DateTime(2016, 1, 1)
let endsAt = DateTime(2016, 2, 1)
let seller = BuyerOrSeller(UserId "x1", "Seller")
let buyer = BuyerOrSeller(UserId "x2", "Buyer")
let auctionOfTyp typ : Auction =
  { id = auctionId
    title = title
    startsAt = startsAt
    expiry = endsAt
    user = seller
    currency=Currency CurrencyCode.SEK
    typ= typ
   }

let auction : Auction = auctionOfTyp (SingleSealedBid Vickrey)

module User=
  let parse = User.__parse
module Amount=
  let parse = Amount.__parse

let sek a =
  { value = a
    currency = Currency CurrencyCode.SEK }

let bid =
    { id = BidId.New()
      auction = auctionId
      user = buyer
      amount = sek 100L
      at = DateTime(2016, 1, 2) }

let buyer1 = BuyerOrSeller(UserId "x2", "Buyer")
let buyer2 = BuyerOrSeller(UserId "x3", "Buyer")

let bid1 = { id = BidId.New()
             auction =auctionId
             user=buyer1
             amount =Amount.parse "SEK10"
             at = startsAt.AddHours(1.0)
           }
let bid2 = { id = BidId.New()
             auction =auctionId
             user=buyer2
             amount =Amount.parse "SEK12"
             at = startsAt.AddHours(2.0)
           }

let addBidsToState state=
  state
  |> S.addBid bid1 |> fst
  |> S.addBid bid2 |> fst

// useful for debugging
let tee on something=
  on something
  something
