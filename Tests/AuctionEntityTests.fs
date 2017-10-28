namespace Tests

open Auctions.Domain
open Auctions
open System
open Xunit

module ``Auction tests`` = 
  let seller = BuyerOrSeller("x1", "Seller") // Support "x1"

  let auction = { id = 1L; startsAt = DateTime(2008,11,25)
                  title = ""
                  endsAt = DateTime(2009,1,1)
                  user = seller 
                  currency=Currency.VAC
                  typ=English { // let's start out with english auctions
                    reservePrice=Amount.parse "VAC0" 
                    minRaise =Amount.parse "VAC0"
                  } 
                }

  [<Fact>]
  let ``date within interval``() = 
    Assert.False( Auction.hasEnded (DateTime(2002,1,1)) auction )

  [<Fact>]
  let ``date just before end``() = 
    Assert.False( Auction.hasEnded (DateTime(2008,12,27)) auction )

  [<Fact>]
  let ``date just after end``() = 
    Assert.True( Auction.hasEnded (DateTime(2009,1,2)) auction )

  let buyer1 = BuyerOrSeller("x2", "Buyer")
  let buyer2 = BuyerOrSeller("x3", "Buyer")

  let bid1 = { id = BidId.NewGuid()
               auction =1L
               user=buyer1
               amount =Amount.parse "VAC10"
               at = DateTime(2008,12,2)
             } 
  let bid2 = { id = BidId.NewGuid()
               auction =1L
               user=buyer2
               amount =Amount.parse "VAC12"
               at = DateTime(2008,12,3)
             } 

  [<Fact>]
  let ``english auction winner and price``() = 
    let maybeAmountAndWinner = Auction.getAmountAndWinner auction [bid1;bid2] (DateTime(2009,1,2))
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``vickrey auction winner and price``() = 
    let maybeAmountAndWinner = Auction.getAmountAndWinner {auction with typ=Vickrey} [bid1;bid2] (DateTime(2009,1,2))
    Assert.Equal(Some(bid1.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``blind auction winner and price``() = 
    let maybeAmountAndWinner = Auction.getAmountAndWinner {auction with typ=Blind} [bid1;bid2] (DateTime(2009,1,2))
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)