module Tests.BlindAuctionStateSpec
open Auctions.Domain
open Xunit
open AuctionStateSpecs
open TestData
let blindAuction = auctionOfTyp (SingleSealedBid Blind)
let blindEmptyState = Auction.emptyState blindAuction

type BlindIncrementSpec() =
    inherit IncrementSpec(blindEmptyState)

[<Fact>]
let ``blind auction winner and price``() =
    let state= blindEmptyState
               |> addBidsToState
               |> S.inc endsAt
    let maybeAmountAndWinner = S.tryGetAmountAndWinner state
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)
