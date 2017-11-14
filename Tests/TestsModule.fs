[<AutoOpen>]
module Tests.TestsModule
open Auctions.Domain
open System

let auctionId : AuctionId = 1L
let title = "auction"
let startsAt = DateTime(2016, 1, 1)
let endsAt = DateTime(2016, 2, 1)
let seller = BuyerOrSeller("x1", "Seller") 
let buyer = BuyerOrSeller("x2", "Buyer")
let auctionOfTyp typ : Auction = 
  { id = auctionId
    title = title
    startsAt = startsAt
    expiry = endsAt
    user = seller
    currency=Currency.SEK
    typ= typ
   }

let auction : Auction = auctionOfTyp (SingleSealedBid Vickrey)


let sek a = 
  { value = a
    currency = Currency.SEK }

let bid = 
    { id = Guid.NewGuid()
      auction = auctionId
      user = buyer
      amount = sek 100L
      at = DateTime(2016, 1, 2) }