namespace Tests

open Auctions.Domain
open Auctions
open System
open Xunit
open FSharpPlus

module ``Auction state tests`` =
  open TestData
  let timedAscAuction = auctionOfTyp (TimedAscending { // let's start out with english auctions
    reservePrice= parse "SEK0"
    minRaise = parse "SEK0"
    timeFrame = TimeSpan.FromSeconds(0.0)
  })
  let timedAscState= Auction.emptyState timedAscAuction
                     |> addBidsToState
                     |> S.inc endsAt
  [<Fact>]
  let ``bid after auction has ended``() =
    Assert.Equal(auctionId |> AuctionHasEnded |> Error, timedAscState |> S.addBid bid |> snd )

  [<Fact>]
  let ``english auction winner and price``() =
    let maybeAmountAndWinner = S.tryGetAmountAndWinner timedAscState
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``vickrey auction winner and price``() =
    let state= auctionOfTyp (SingleSealedBid Vickrey)
               |> Auction.emptyState
               |> addBidsToState
               |> S.inc endsAt
    let maybeAmountAndWinner = S.tryGetAmountAndWinner state
    Assert.Equal(Some(bid1.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``blind auction winner and price``() =
    let state= auctionOfTyp (SingleSealedBid Blind)
               |> Auction.emptyState
               |> addBidsToState
               |> S.inc endsAt
    let maybeAmountAndWinner = S.tryGetAmountAndWinner state
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``english auction Can't place bid lower than highest bid``() =
    // setup
    let (state,res) = Auction.emptyState timedAscAuction
                       |> S.addBid {bid with at=startsAt.AddHours(1.0)}
    let nextBid = {bid with at=startsAt.AddHours(2.0)}

    // act
    let res = S.addBid nextBid state |> snd
    //printf "%A" res
    Assert.Equal(Error(MustPlaceBidOverHighestBid bid.amount), res)

  [<Fact>]
  let ``date within interval``() =
    let state = Auction.emptyState timedAscAuction
                |> S.inc (startsAt.AddHours(1.0))
    Assert.False(S.hasEnded state)

  [<Fact>]
  let ``date just before end``() =
    let state = Auction.emptyState timedAscAuction
                |> S.inc (endsAt.AddHours(-1.0))
    Assert.False(S.hasEnded state)

  [<Fact>]
  let ``date just after end``() =
    let state = Auction.emptyState timedAscAuction
                |> S.inc (endsAt.AddHours(1.0))
    Assert.True(S.hasEnded state)

