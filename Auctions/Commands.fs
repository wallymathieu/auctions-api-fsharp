module Auctions.Commands

open Domain
open System
open Either

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
    | AddAuction(_,auction) -> Some auction.id
    | PlaceBid(_,bid) -> Some bid.auction

type CommandSuccess = 
  | AuctionAdded of DateTime * Auction
  | BidAccepted of DateTime * Bid

let validateBid (auction : Auction) (at:DateTime,bid : Bid) = 
  if at > auction.endsAt then Error(AuctionHasEnded auction.id)
  else if bid.user = auction.user then Error(SellerCannotPlaceBids(User.getId bid.user, auction.id))
  else Ok()
open Repo
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
        yield! r.SaveAuction auction
        yield AuctionAdded (at, auction)
    }
  | PlaceBid(at, bid) -> 
    either { 
      let! auction = r.GetAuction bid.auction
      return! (match r.TryGetBid bid.id with
               | None -> 
                 either { 
                   do! validateBid auction (at, bid)
                   yield! r.SaveBid bid
                   yield BidAccepted (at, bid)
                 }
               | Some _ -> Error(BidAlreadyExists bid.id))
    }
