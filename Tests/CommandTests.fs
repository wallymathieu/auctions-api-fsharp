namespace Tests

open Auctions.Domain
open Auctions.Commands
open Auctions
open System
open Xunit

module ``Bid tests`` = 
  let auctionId : AuctionId = 1L
  let title = "auction"
  let startsAt = DateTime(2016, 1, 1)
  let endsAt = DateTime(2016, 2, 1)
  let seller = BuyerOrSeller(Guid.NewGuid(), "seller")
  let buyer = BuyerOrSeller(Guid.NewGuid(), "buyer")
  
  let auction : Auction = 
    { id = auctionId
      title = title
      startsAt = startsAt
      endsAt = endsAt
      user = seller }
  
  let sek a = 
    { value = a
      currency = "SEK" }
  
  [<Fact>]
  let ``Seller cant bid``() = 
    // setup
    let r = ConcurrentRepository() :> IRepository
    handleCommand r (AddAuction(startsAt, auction)) |> ignore
    let d = DateTime(2016, 1, 2)
    
    // act
    let res = 
      handleCommand r (PlaceBid(d, 
                                { id = Guid.NewGuid()
                                  auction = auctionId
                                  user = seller
                                  amount = sek 100.0
                                  at = d }))
    // assert
    Assert.Equal(Error(SellerCannotPlaceBids(User.getId seller, auctionId)), res)
    Assert.True(r.GetBidsForAuction(auctionId).IsEmpty)
  
  [<Fact>]
  let ``Buyer can place bid``() = 
    // setup
    let r = ConcurrentRepository() :> IRepository
    handleCommand r (AddAuction(startsAt, auction)) |> ignore
    let d = DateTime(2016, 1, 2)
    
    let bid = 
      d, 
      { id = Guid.NewGuid()
        auction = auctionId
        user = buyer
        amount = sek 100.0
        at = d }
    
    // act
    let res = handleCommand r (PlaceBid bid) |> Result.map snd
    // assert
    Assert.Equal(Ok(BidAccepted bid), res)
    Assert.False(r.GetBidsForAuction(auctionId).IsEmpty)
