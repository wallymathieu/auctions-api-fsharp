namespace Tests

open Auctions.Domain
open Auctions
open Auctions.Actors

open System
open Xunit

module ``Auction agent tests`` = 
  let seller = BuyerOrSeller("x1", "Seller")

  let auction = { id = 1L; startsAt = DateTime(2008,11,25)
                  title = ""
                  expiry = DateTime(2009,1,1)
                  user = seller 
                  currency=Currency.VAC
                  typ=TimedAscending { // let's start out with english auctions
                    reservePrice=Amount.parse "VAC0" 
                    minRaise =Amount.parse "VAC0"
                    timeFrame = TimeSpan.FromSeconds(0.0)
                  } 
                }

  let buyer = BuyerOrSeller("x2", "Buyer")

  let emptyHandler= ignore
  let validBid = { id = BidId.NewGuid()
                   auction =1L
                   user=buyer
                   amount =Amount.parse "VAC10"
                   at = DateTime(2008,12,1)
                 } 
  [<Fact>]
  let ``create auction with bid, and wait for the end of the auction``() = 
    let mutable t = DateTime(2008,11,24)
    let time () = t
    let d = AuctionDelegator.create ([], emptyHandler, time)
    let res=Async.RunSynchronously( d.UserCommand (AddAuction (t,auction)) )
    Assert.Equal(Ok (AuctionAdded (t, auction)), res)
    t <- t.AddDays(0.5)
    let res=Async.RunSynchronously( d.UserCommand (PlaceBid (t,validBid)) )
    Assert.Equal(Ok (BidAccepted (t, validBid)), res)
    t <- auction.expiry.AddDays(1.5)
    //Async.RunSynchronously(d.WakeUp())
    let maybeAuctionAndBids = Async.RunSynchronously( d.GetAuction auction.id )
    //printfn "++++++++++++++++++++++++\n%A\n++++++++++++++++++++++++" maybeAuctionAndBids
    Assert.Equal( (Some (auction, [validBid], (Some (validBid.amount, buyer)))),maybeAuctionAndBids)
