namespace Auctions

open System.Collections.Concurrent
open System.Collections.Generic
open Auctions.Domain

[<Interface>]
type IRepository = 
  abstract Auctions : unit -> Auction list
  abstract TryGetAuction : AuctionId -> Auction option
  abstract TryGetBid : AuctionId * BidId -> Bid option
  abstract SaveAuction : Auction -> Result<IRepository, Errors>
  abstract SaveBid : Bid -> Result<IRepository, Errors>
  abstract GetBidsForAuction : AuctionId -> Bid list

module Repo = 
  type R = IRepository
  
  type IRepository with
    
    member this.GetAuction auctionId = 
      match this.TryGetAuction auctionId with
      | Some value -> Ok(value)
      | None -> Error(UnknownAuction(auctionId))
    
  
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
    member this.TryGetAuction auctionId = this.auctions |> List.tryFind (fun a -> a.id = auctionId)
    member this.TryGetBid(auctionId,bidId) = this.bids |> List.tryFind (fun b -> b.id = bidId && b.auction = auctionId)
    member this.SaveAuction auction = Ok({ this with auctions = auction :: this.auctions } :> IRepository)
    member this.SaveBid bid = Ok({ this with bids = bid :: this.bids } :> IRepository)
    member this.GetBidsForAuction auctionId = this.bids |> List.filter (fun b -> b.auction = auctionId)
  static member empty = {auctions=[]; bids=[] }

/// Concurrent version of repository.
type MutableRepository() = 
  // in memory repository
  let auctions = new Dictionary<AuctionId, Auction>()
  let auctionBids = new Dictionary<AuctionId, Dictionary<BidId,Bid>>()
  interface IRepository with
    member this.Auctions() = auctions.Values |> Seq.toList
    member this.TryGetAuction auctionId = Dic.tryGet auctionId auctions

    member this.TryGetBid(auctionId,bidId) = 
      Dic.tryGet auctionId auctionBids 
      |> Option.bind (fun bids-> Dic.tryGet bidId bids)
    
    member this.SaveBid bid = 
      let bids = auctionBids |> Dic.addOrUpdate bid.auction (new Dictionary<BidId,Bid>()) (fun bag -> bag)
      bids.Add(bid.id, bid) |> ignore
      Ok(this :> IRepository)
    
    member this.SaveAuction auction = 
      auctions |> Dic.addOrUpdate (Auction.getId auction) auction (fun _ -> auction) |> ignore
      Ok(this :> IRepository)
    
    member this.GetBidsForAuction(auctionId : AuctionId) = 
      match auctionBids.TryGetValue(auctionId) with
      | true, value -> value.Values |> Seq.toList
      | false, _ -> []
