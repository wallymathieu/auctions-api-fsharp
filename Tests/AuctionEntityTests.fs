namespace Tests

open Auctions.Domain
open Auctions
open System
open Xunit

module ``Auction tests`` = 
  let auction = { id = 1L; startsAt = DateTime(2001,1,1)
                  title = ""
                  endsAt = DateTime(2009,1,1)
                  user =  Support "x1" 
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