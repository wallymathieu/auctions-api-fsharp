module Auctions.Commands
open Either
open Domain
open System
open Repo

type Command = 
  | AddAuction of DateTime * Auction
  | PlaceBid of DateTime * Bid
  
  /// the time when the command was issued
  static member getAt command = 
    match command with
    | AddAuction(at, _) -> at
    | PlaceBid(at, _) -> at
  
  static member getAuction command = 
    match command with
    | AddAuction(_,auction) -> auction.id
    | PlaceBid(_,bid) -> bid.auction

type CommandSuccess = 
  | AuctionAdded of DateTime * Auction
  | BidAccepted of DateTime * Bid


/// the intention of this function is to return the modified repository and a success, or return an error
/// I.e. this function does not know if it's an immutable repository 
let handleCommand (r : IRepository) command : Result<IRepository * CommandSuccess, Errors> = 
  match command with
  | AddAuction(at, auction) -> 
    let id = auction.id
    either { 
      match r.TryGetAuction id with
      | Some _ -> return! Error(AuctionAlreadyExists id)
      | None -> 
        let! res= r.SaveAuction auction
        return res,AuctionAdded (at, auction)
    }
  | PlaceBid(at, bid) -> 
    either { 
      let! auction = r.GetAuction bid.auction
      return! (match r.TryGetBid bid.id with
               | None -> 
                 either { 
                   do! validateBid auction bid
                   do! validateBidForAuctionType auction (r.GetBidsForAuction auction.id) bid
                   let! res= r.SaveBid bid
                   return res,BidAccepted (at, bid)
                 }
               | Some _ -> Error(BidAlreadyExists bid.id))
    }
