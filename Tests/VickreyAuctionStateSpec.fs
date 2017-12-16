module Tests.VickreyAuctionStateSpec
open Auctions.Domain
open AuctionStateSpecs
open Xunit
open System
let vickreyAuction = auctionOfTyp (SingleSealedBid Vickrey)
let vickreyEmptyState = Auction.emptyState vickreyAuction
type VickreyIncrementSpec() = 
    inherit IncrementSpec(vickreyEmptyState)

[<Fact>]
let ``vickrey auction winner and price``() = 
    let state= vickreyEmptyState
               |> addBidsToState
               |> S.inc endsAt
    let maybeAmountAndWinner = S.tryGetAmountAndWinner state 
    Assert.Equal(Some(bid1.amount,bid2.user),maybeAmountAndWinner)
