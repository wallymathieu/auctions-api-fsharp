namespace Tests

open Auctions.Domain
open Auctions
open Auctions.Actors
open FSharpPlus
open System
open Xunit
open FsUnit.Xunit

module ``Auction agent tests`` =
  open TestData
  //domain specific matchers:
  open NHamcrest.Core
  let equalError x = CustomMatcher<obj>(sprintf "Equals Error %A" x, fun (a:obj)->match a :?> Result<Event,Errors> with | Error err-> err= x | Ok _ -> false)
  let equalOk x = CustomMatcher<obj>(sprintf "Equals Ok %A" x, fun (a:obj)-> match a :?> Result<Event,Errors> with | Error _-> false | Ok ok -> ok =x)

  let seller = BuyerOrSeller(UserId "x1", "Seller")

  let auction = { id = auctionId; startsAt = DateTime(2008,11,25)
                  title = ""
                  expiry = DateTime(2009,1,1)
                  user = seller
                  currency=Currency.VAC
                  typ=TimedAscending { // let's start out with english auctions
                    reservePrice=parse "VAC0"
                    minRaise =parse "VAC0"
                    timeFrame = TimeSpan.FromSeconds(0.0)
                  }
                }

  let buyer = BuyerOrSeller(UserId "x2", "Buyer")

  let emptyHandler= ignore
  let validBid = { auction =auctionId
                   user=buyer
                   amount =parse "VAC10"
                   at = DateTime(2008,12,1)
                 }
  [<Fact>]
  let ``create auction with bid, and wait for the end of the auction``() =
    let mutable t = DateTime(2008,11,24)
    let time () = t
    let d = AuctionDelegator.create ([], emptyHandler, time, ignore)
    Async.RunSynchronously( d.UserCommand (AddAuction (t, auction)) )
    |> should equalOk (AuctionAdded (t, auction))
    t <- t.AddDays(0.5)
    Async.RunSynchronously( d.UserCommand (PlaceBid (t, validBid)) )
    |> should equalOk (BidAccepted (t, validBid))
    t <- auction.expiry.AddDays(1.5)
    //Async.RunSynchronously(d.WakeUp())
    let maybeAuctionAndBids = Async.RunSynchronously( d.GetAuction auction.id )
    maybeAuctionAndBids |> should equal (Some (auction, [validBid], (Some (validBid.amount, buyer))))

  [<Fact>]
  let ``try to create auction in the past``() =
    let mutable t = DateTime(2008,11,24)
    let time () = t
    let d = AuctionDelegator.create ([], emptyHandler, time, ignore)
    Async.RunSynchronously( d.UserCommand (AddAuction (t, {auction with expiry = t.AddDays(-1.0) })) )
    |> should equalError (AuctionHasEnded (auction.id))

  [<Fact>]
  let ``try to create auction when there is already an auction``() =
    let mutable t = DateTime(2008,11,24)
    let time () = t
    let d = AuctionDelegator.create ([], emptyHandler, time, ignore)
    Async.RunSynchronously( d.UserCommand (AddAuction (t, auction)) )
    |> ignore
    Async.RunSynchronously( d.UserCommand (AddAuction (t, auction)) )
    |> should equalError (AuctionAlreadyExists (auction.id))
