namespace Tests
open Auctions.Domain
open Auctions
open System
open Xunit

module ``Auction validate bid tests`` = 

  [<Fact>]
  let ``Seller cant bid``() = 
    // setup
    let validateBid = fun b-> Auction.validateBid b auction
    let d = DateTime(2016, 1, 2)
    
    // act
    let res = 
      validateBid ({ bid with user = seller; at = d })
    // assert
    Assert.Equal(Error(SellerCannotPlaceBids(User.getId seller, auctionId)), res)
  
  [<Fact>]
  let ``Buyer can place bid``() = 
    // setup
    let validateBid = fun b-> Auction.validateBid b auction
    let d = DateTime(2016, 1, 2)

    // act
    let res = 
      validateBid { bid with at = d }
    // assert
    Assert.Equal(Ok(), res)

