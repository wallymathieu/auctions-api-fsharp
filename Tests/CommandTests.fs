namespace Tests
open Auctions.Domain
open Auctions.Commands
open Auctions
open System
open Xunit

module ``Bid tests`` = 
  let auctionId = Guid.NewGuid()
  let title = "auction"
  let startsAt = DateTime(2016,1,1)
  let endsAt = DateTime(2016,2,1)
  let seller = BuyerOrSeller(Guid.NewGuid(), "seller")
  let buyer = BuyerOrSeller(Guid.NewGuid(), "buyer")
  let auction:Auction={id = auctionId; title = title; startsAt = startsAt; endsAt = endsAt; user = seller}
  let sek a = { value=a; currency="SEK" }

  [<Fact>]
  let ``Seller cant bid``() = 
    // setup
    let r = ConcurrentRepository() :>IRepository
    handleCommand r (AddAuction (startsAt, auction)) |> ignore
    // act
    let res = handleCommand r (PlaceBid { 
            id=Guid.NewGuid() 
            auction=auctionId
            at=DateTime(2016,1,2)
            user=seller
            amount=sek 100.0 
    })
    // assert
    Assert.Equal(Error (SellerCannotPlaceBids(User.getId seller,auctionId)),res)
    Assert.True(r.GetBidsForAuction(auctionId).IsEmpty)

  [<Fact>]
  let ``Buyer can place bid``() = 
    // setup
    let r = ConcurrentRepository() :>IRepository
    handleCommand r (AddAuction (startsAt, auction)) |> ignore
    let bid ={ 
            id=Guid.NewGuid() 
            auction=auctionId
            at=DateTime(2016,1,2)
            user=buyer
            amount=sek 100.0 
            }
    // act
    let res = handleCommand r (PlaceBid bid)
    // assert
    Assert.Equal(Ok (BidAccepted bid),res)
    Assert.False(r.GetBidsForAuction(auctionId).IsEmpty)
