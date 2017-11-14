namespace Tests

open Auctions.Domain
open Auctions
open System
open Xunit

module ``Auction Bid tests`` = 
  let validBid = { id = BidId.NewGuid()
                   auction =1L
                   user=buyer
                   amount =Amount.parse "SEK10"
                   at = DateTime(2008,12,1)
                 } 
  let bidWithSameUser = { validBid with user=seller } 
  let bidAfterAuctionEnded = { validBid with at = DateTime(2009,1,2) } 
  let validateBid = fun b-> Auction.validateBid b auction
  [<Fact>]
  let ``valid bid``() = 
    Assert.Equal( Ok(), validateBid validBid )

  [<Fact>]
  let ``seller bidding on auction``() = 
    Assert.Equal( Error (SellerCannotPlaceBids ("x1",1L)), validateBid bidWithSameUser )

