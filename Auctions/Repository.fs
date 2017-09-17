namespace Auctions

open System.Collections.Concurrent
open Auctions.Domain

[<Interface>]
type IRepository = 
    abstract GetAuction : AuctionId -> Result<Auction, Error>
    abstract TryGetAuction : AuctionId -> Auction option
    abstract SaveAuction : Auction -> Result<unit, Error>
    abstract GetBid : BidId -> Result<Bid, Error>
    abstract TryGetBid : BidId -> Bid option
    abstract SaveBid : Bid -> Result<unit, Error>
    abstract GetBidsForAuction : AuctionId -> Bid list

/// Concurrent version of repository.
type ConcurrentRepository() = 
    // in memory repository
    let auctions = new ConcurrentDictionary<AuctionId, Auction>()
    let bids = new ConcurrentDictionary<BidId, Bid>()
    let auctionBids = new ConcurrentDictionary<AuctionId, ConcurrentBag<BidId>>()
    interface IRepository with
        
        member this.GetAuction auctionId = 
            match auctions.TryGetValue(auctionId) with
            | true, value -> Ok(value)
            | false, _ -> Error(UnknownAuction(auctionId))
        
        member this.GetBid bidId = 
            match bids.TryGetValue(bidId) with
            | true, value -> Ok(value)
            | false, _ -> Error(UnknownBid(bidId))
        
        member this.TryGetBid bidId = 
            match bids.TryGetValue(bidId) with
            | true, value -> Some(value)
            | false, _ -> None
        
        member this.TryGetAuction auctionId = 
            match auctions.TryGetValue(auctionId) with
            | true, value -> Some(value)
            | false, _ -> None
        
        member this.SaveBid bid = 
            let bidIds = auctionBids.AddOrUpdate(bid.auction.id, new ConcurrentBag<BidId>(), (fun key bag -> bag))
            bidIds.Add(bid.id)
            bids.AddOrUpdate(Bid.getId bid, bid, (fun key oldvalue -> bid)) |> ignore
            Ok()
        
        member this.SaveAuction auction = 
            auctions.AddOrUpdate(Auction.getId auction, auction, (fun key oldvalue -> auction)) |> ignore
            Ok()
        
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
