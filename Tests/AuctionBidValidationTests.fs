namespace Tests

open Auctions.Domain
open Auctions.Commands
open Auctions
open System
open Xunit

module ``Auction Bid tests`` = 
  let seller = BuyerOrSeller("x1", "Seller") // Support "x1"
  let buyer = BuyerOrSeller("x2", "Buyer")
  let auction = { id = 1L; startsAt = DateTime(2001,1,1)
                  title = "auction"
                  endsAt = DateTime(2009,1,1)
                  user = seller 
                  currency=Currency.VAC
                  typ=English { // let's start out with english auctions
                    reservePrice=Amount.parse "VAC0" 
                    minRaise =Amount.parse "VAC0"
                  } 
                }
  let validBid = { id = BidId.NewGuid()
                   auction =1L
                   user=buyer
                   amount =Amount.parse "VAC10"
                   at = DateTime(2008,12,1)
                 } 
  let bidWithSameUser = { validBid with user=seller } 
  let bidAfterAuctionEnded = { validBid with at = DateTime(2009,1,2) } 
  let validateBid = validateBid auction
  [<Fact>]
  let ``valid bid``() = 
    Assert.Equal( Ok(), validateBid validBid )

  [<Fact>]
  let ``seller bidding on auction``() = 
    Assert.Equal( Error (SellerCannotPlaceBids ("x1",1L)), validateBid bidWithSameUser )

  [<Fact>]
  let ``bid after auction has ended``() = 
    Assert.Equal( Error (AuctionHasEnded 1L), validateBid bidAfterAuctionEnded )
