namespace Tests

open Auctions.Domain
open Auctions.Commands
open Auctions
open System
open Xunit

module ``Bid commands tests`` = 
  let auctionId : AuctionId = 1L
  let title = "auction"
  let startsAt = DateTime(2016, 1, 1)
  let endsAt = DateTime(2016, 2, 1)
  let seller = BuyerOrSeller(Guid.NewGuid().ToString("N"), "seller")
  let buyer = BuyerOrSeller(Guid.NewGuid().ToString("N"), "buyer")
  
  let auction : Auction = 
    { id = auctionId
      title = title
      startsAt = startsAt
      endsAt = endsAt
      user = seller
      currency=Currency.SEK
      typ=English { // let's start out with english auctions
        reservePrice=Amount.parse "SEK0" 
        minRaise =Amount.parse "SEK0"
      } 
     }

  let sek a = 
    { value = a
      currency = Currency.SEK }

  let bid = 
      { id = Guid.NewGuid()
        auction = auctionId
        user = buyer
        amount = sek 100L
        at = DateTime(2016, 1, 2) }
  
  [<Fact>]
  let ``Seller cant bid``() = 
    // setup
    let r = MutableRepository() :> IRepository
    handleCommand r (AddAuction(startsAt, auction)) |> ignore
    let d = DateTime(2016, 1, 2)
    
    // act
    let res = 
      handleCommand r (PlaceBid(d, 
                                { bid with user = seller; at = d }))
    // assert
    Assert.Equal(Error(SellerCannotPlaceBids(User.getId seller, auctionId)), res)
    Assert.True(r.GetBidsForAuction(auctionId).IsEmpty)
  
  [<Fact>]
  let ``Buyer can place bid``() = 
    // setup
    let r = MutableRepository() :> IRepository
    handleCommand r (AddAuction(startsAt, auction)) |> ignore
    let d = DateTime(2016, 1, 2)
    
    let bid = d, { bid with at = d }
    
    // act
    let res = handleCommand r (PlaceBid bid) |> Result.map snd
    // assert
    Assert.Equal(Ok(BidAccepted bid), res)
    Assert.False(r.GetBidsForAuction(auctionId).IsEmpty)

  [<Fact>]
  let ``Can't place bid lower than highest bid``() = 
    // setup
    let r = MutableRepository() :> IRepository
    handleCommand r (AddAuction(startsAt, auction)) |> ignore

    handleCommand r (PlaceBid (DateTime(2016, 1, 2),bid)) |> ignore
    let d2 = DateTime(2016, 1, 3)
    let nextBid = d2, {bid with at=d2; id=Guid.NewGuid()}
    
    // act
    let res = handleCommand r (PlaceBid nextBid) |> Result.map snd
    // assert
    //printf "%A" res
    Assert.Equal(Error(MustPlaceBidOverHighestBid bid.amount), res)
    