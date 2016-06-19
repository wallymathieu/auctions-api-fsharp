namespace Auctions
open System.Collections.Concurrent
open Domain
open Either

type Repository()=
    // in memory repository
    let auctions= new ConcurrentDictionary<AuctionId, Auction>()
    let bids= new ConcurrentDictionary<BidId, Bid>()
    let auctionBids= new ConcurrentDictionary<AuctionId, ConcurrentBag<BidId>>()
    
    member this.GetAuction auctionId=
        match auctions.TryGetValue(auctionId) with
        | true,value->Success(value)
        | false,_->Failure(UnknownAuction(auctionId))

    member this.GetBid bidId=
        match bids.TryGetValue(bidId) with
        | true,value->Success(value)
        | false,_->Failure(UnknownBid(bidId))

    member this.TryGetBid bidId=
        match bids.TryGetValue(bidId) with
        | true,value->Some(value)
        | false,_->None

    member this.TryGetAuction auctionId=
        match auctions.TryGetValue(auctionId) with
        | true,value->Some(value)
        | false,_->None

    member this.SaveBid bid=
        let bidIds = auctionBids.AddOrUpdate(bid.auction.id, new ConcurrentBag<BidId>(), (fun key bag-> bag))
        bidIds.Add(bid.id) 
        bids.AddOrUpdate(Bid.getId bid, bid, (fun key oldvalue->bid)) |> ignore
        Success()

    member this.SaveAuction auction=
        auctions.AddOrUpdate(Auction.getId auction, auction, (fun key oldvalue->auction)) |> ignore
        Success()

    member this.GetBidsForAuction (auctionId:AuctionId)=
        match auctionBids.TryGetValue(auctionId) with
        | true, value -> value |> Seq.toList
        | false, _ -> []

