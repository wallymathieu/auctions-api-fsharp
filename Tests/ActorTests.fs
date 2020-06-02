namespace Tests

open Auctions.Domain
open Auctions
open Auctions.Actors
open Hopac
open System
open Xunit
open System.Globalization
open FSharpPlus
open FsUnit.Xunit

module ``Auction agent tests`` =
  //domain specific matchers:
  open NHamcrest.Core
  let equalError x = CustomMatcher<obj>(sprintf "Equals Error %A" x, fun (a:obj)->match a :?> Result<CommandSuccess,Errors> with | Error err-> err= x | Ok _ -> false)
  let equalOk x = CustomMatcher<obj>(sprintf "Equals Ok %A" x, fun (a:obj)-> match a :?> Result<CommandSuccess,Errors> with | Error _-> false | Ok ok -> ok =x)

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

  let emptyHandler= fun _ -> Job.unit()
  let validBid = { id = BidId.New()
                   auction =auctionId
                   user=buyer
                   amount =parse "VAC10"
                   at = DateTime(2008,12,1)
                 }
  [<Fact>]
  let ``create auction with bid, and wait for the end of the auction``() =
    let j = job{
      let mutable t = DateTime(2008,11,24)
      let time () = t
      let! d =  AuctionDelegator.create ([], emptyHandler, time, fun _ -> Job.unit())
      let! res= d.UserCommand (AddAuction (t,auction))
      Assert.Equal(Ok (AuctionAdded (t, auction)), res)
      t <- t.AddDays(0.5)
      let! res= d.UserCommand (PlaceBid (t,validBid))
      Assert.Equal(Ok (BidAccepted (t, validBid)), res)
      t <- auction.expiry.AddDays(1.5)
      do! d.WakeUp()
      // ensure that wake up call goes through
      do! timeOut (TimeSpan.FromSeconds 0.1)
      let! maybeAuctionAndBids = d.GetAuction auction.id
      //printfn "++++++++++++++++++++++++\n%A\n++++++++++++++++++++++++" maybeAuctionAndBids
      return Assert.Equal( (Some (auction, [validBid], (Some (validBid.amount, buyer)))),maybeAuctionAndBids)
    }
    run j
  [<Fact>]
  let ``create auction with bid, and ping until the end of the auction``() =
    let j = job{
      let mutable t = DateTime(2008,11,24)
      let time () = t
      let! d =  AuctionDelegator.create ([], emptyHandler, time, fun _ -> Job.unit())
      let! res= d.UserCommand (AddAuction (t,auction))
      Assert.Equal(Ok (AuctionAdded (t, auction)), res)
      t <- t.AddDays(0.5)
      let! res= d.UserCommand (PlaceBid (t,validBid))
      Assert.Equal(Ok (BidAccepted (t, validBid)), res)
      t <- auction.expiry.AddDays(1.5)

      do! Job.Ignore <| d.GetAuction auction.id
      // ensure that the first get auction ping goes through
      do! timeOut (TimeSpan.FromSeconds 0.1)

      let! maybeAuctionAndBids = d.GetAuction auction.id
      return Assert.Equal( (Some (auction, [validBid], (Some (validBid.amount, buyer)))),maybeAuctionAndBids)
    }
    run j

  [<Fact>]
  let ``try to create auction in the past``() =
    let j = job{
      let mutable t = DateTime(2008,11,24)
      let time () = t
      let! d = AuctionDelegator.create ([], emptyHandler, time, fun _ -> Job.unit())
      let! res= d.UserCommand (AddAuction (t, {auction with expiry = t.AddDays(-1.0) }))
      return res |> should equalError (AuctionHasEnded (auction.id))
    }
    run j

  [<Fact>]
  let ``try to create auction when there is already an auction``() =
    let j = job{
      let mutable t = DateTime(2008,11,24)
      let time () = t
      let! d = AuctionDelegator.create ([], emptyHandler, time, fun _ -> Job.unit())
      do! Job.Ignore <|  d.UserCommand (AddAuction (t, auction))
      let! res= d.UserCommand (AddAuction (t, auction))
      return res |> should equalError (AuctionAlreadyExists (auction.id))
    }
    run j
