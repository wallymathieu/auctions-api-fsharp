namespace Auctions

open System.Collections.Concurrent
open System.Collections.Generic
open Auctions.Domain

[<Interface>]
type IRepository = 
  abstract Auctions : unit -> Auction list
  abstract TryGetAuction : AuctionId -> Auction option
  abstract SaveAuction : Auction -> Result<IRepository, Errors>
  abstract TryGetBid : BidId -> Bid option
  abstract SaveBid : Bid -> Result<IRepository, Errors>
  abstract GetBidsForAuction : AuctionId -> Bid list

module Repo = 
  type R = IRepository
  
  type IRepository with
    
    member this.GetAuction auctionId = 
      match this.TryGetAuction auctionId with
      | Some value -> Ok(value)
      | None -> Error(UnknownAuction(auctionId))
    
    member this.GetBid bidId = 
      match this.TryGetBid bidId with
      | Some value -> Ok(value)
      | None -> Error(UnknownBid(bidId))
  
  let auctions (r : R) = r.Auctions()
  let getAuction (r : R) = r.GetAuction
  let getAuctionBids (r : R) = r.GetBidsForAuction
  let saveAuction a (r : R) = r.SaveAuction a
  let saveBid b (r : R) = r.SaveBid b

/// Immutable version of the repository
type ImmutableRepository = 
  { auctions : Auction list
    bids : Bid list }
  interface IRepository with
    member this.Auctions() = this.auctions
    member this.TryGetBid bidId = this.bids |> List.tryFind (fun b -> b.id = bidId)
    member this.TryGetAuction auctionId = this.auctions |> List.tryFind (fun a -> a.id = auctionId)
    member this.SaveAuction auction = Ok({ this with auctions = auction :: this.auctions } :> IRepository)
    member this.SaveBid bid = Ok({ this with bids = bid :: this.bids } :> IRepository)
    member this.GetBidsForAuction auctionId = this.bids |> List.filter (fun b -> b.auction = auctionId)

/// Concurrent version of repository.
type ConcurrentRepository() = 
  let tryGet (c : IDictionary<_, _>) k = 
    match c.TryGetValue(k) with
    | true, value -> Some(value)
    | false, _ -> None

  // in memory repository
  let auctions = new ConcurrentDictionary<AuctionId, Auction>()
  let bids = new ConcurrentDictionary<BidId, Bid>()
  let auctionBids = new ConcurrentDictionary<AuctionId, ConcurrentBag<BidId>>()
  interface IRepository with
    member this.Auctions() = auctions.Values |> Seq.toList
    member this.TryGetBid bidId = tryGet bids bidId
    member this.TryGetAuction auctionId = tryGet auctions auctionId
    
    member this.SaveBid bid = 
      let bidIds = auctionBids.AddOrUpdate(bid.auction, new ConcurrentBag<BidId>(), (fun key bag -> bag))
      bidIds.Add(bid.id)
      bids.AddOrUpdate(Bid.getId bid, bid, (fun key oldvalue -> bid)) |> ignore
      Ok(this :> IRepository)
    
    member this.SaveAuction auction = 
      auctions.AddOrUpdate(Auction.getId auction, auction, (fun key oldvalue -> auction)) |> ignore
      Ok(this :> IRepository)
    
    member this.GetBidsForAuction(auctionId : AuctionId) = 
      match auctionBids.TryGetValue(auctionId) with
      | true, value -> 
        value
        |> Seq.choose (fun bidId -> 
             match bids.TryGetValue bidId with
             | true, value -> Some value
             | false, _ -> None)
        |> Seq.toList
      | false, _ -> []
