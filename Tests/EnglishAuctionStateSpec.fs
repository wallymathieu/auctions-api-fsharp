module Tests.EnglishAuctionStateSpec
open Auctions.Domain
open AuctionStateSpecs
open Xunit
open System
let timedAscAuction = auctionOfTyp (TimedAscending { // let's start out with english auctions
    reservePrice=Amount.tryParse "SEK0" |> Option.get
    minRaise =Amount.tryParse "SEK0" |> Option.get
    timeFrame = TimeSpan.FromSeconds(0.0)
  })
let englishEmptyState = Auction.emptyState timedAscAuction
  
type EnglishIncrementSpec() = 
    inherit IncrementSpec(englishEmptyState)

let timedAscState= englishEmptyState
                     |> addBidsToState
                     |> S.inc endsAt
[<Fact>]
let ``bid after auction has ended``() = 
    Assert.Equal( Error (AuctionHasEnded 1L), timedAscState |> S.addBid bid |> snd )

[<Fact>]
let ``english auction winner and price``() = 
    let maybeAmountAndWinner = S.tryGetAmountAndWinner timedAscState 
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)


[<Fact>]
let ``english auction Can't place bid lower than highest bid``() = 
    // setup
    let (state,res) = englishEmptyState
                       |> S.addBid {bid with at=startsAt.AddHours(1.0)}
    let nextBid = {bid with at=startsAt.AddHours(2.0); id=Guid.NewGuid()}
    
    // act
    let res = S.addBid nextBid state |> snd 
    //printf "%A" res
    Assert.Equal(Error(MustPlaceBidOverHighestBid bid.amount), res)

