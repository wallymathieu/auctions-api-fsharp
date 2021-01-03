namespace Tests

open Auctions.Domain
open System
open Xunit
open FSharpPlus
open TestData
module ``Auction Bid tests`` =
  let validBid = { id = BidId.New()
                   auction =auctionId
                   user=buyer
                   amount =parse "SEK10"
                   at = auction.startsAt.AddHours(1.0)
                 }
  let bidWithSameUser = { validBid with user=seller }
  let bidAfterAuctionEnded = { validBid with at = auction.expiry.AddHours(1.0) }
  let bidBeforeAuctionStarted = { validBid with at = auction.startsAt.AddHours(-1.0) }
  let validateBid = fun b-> Auction.validateBid b auction
  [<Fact>]
  let ``valid bid``() =
    Assert.Equal( Ok(), validateBid validBid )

  [<Fact>]
  let ``seller bidding on auction``() =
    Assert.Equal( Error (SellerCannotPlaceBids (UserId "x1",auctionId)), validateBid bidWithSameUser )

  [<Fact>]
  let ``bid after auction has ended``() =
    Assert.Equal( Error (AuctionHasEnded auctionId), validateBid bidAfterAuctionEnded )

  [<Fact>]
  let ``bid before auction has started``() =
    Assert.Equal( Error (AuctionHasNotStarted auctionId), validateBid bidBeforeAuctionStarted )
