module Auctions.Commands
open Domain
open System
open Either

type Command = 
  | Empty of at : DateTime
  | AddAuction of  at :DateTime * auction: Auction
  | PlaceBid of bid : Bid
  /// the time when the command was issued
  static member getAt command = 
    match command with
    | AddAuction(at = at) -> at
    | PlaceBid(bid) -> bid.at
    | Empty(at = at) -> at
  static member getAuction command =
    match command with
    | AddAuction(auction = auction) -> Some auction.id
    | PlaceBid(bid = bid) -> Some bid.auction
    | Empty _ -> None

type CommandSuccess =
    | NoCommand
    | AuctionAdded of Auction
    | BidAccepted of Bid

let validateBid (auction:Auction) (bid:Bid)=
  if bid.at > auction.endsAt then Error(AuctionHasEnded auction.id)
  else if bid.user = auction.user then Error(SellerCannotPlaceBids(User.getId bid.user, auction.id))
  else Ok ()



let handleCommand (r : IRepository) command : Result<CommandSuccess,Errors> = 
  match command with
  | Empty(at = at) -> Ok( NoCommand )
  | AddAuction (auction=auction) -> 
    let id = auction.id
    either { 
      match r.TryGetAuction id with
      | Some _ -> return! Error(AuctionAlreadyExists id)
      | None -> 
        do! r.SaveAuction auction
        return AuctionAdded auction
    }
  | PlaceBid (bid) -> 
    either { 
      let! auction = r.GetAuction bid.auction
      return! (
        match r.TryGetBid bid.id with
        | None -> 
          //if bid.at > auction.endsAt then Error(AuctionHasEnded auction.id)
          //else if bid.user = auction.user then Error(SellerCannotPlaceBids(User.getId bid.user, auction.id))
          //else 
            either{
               do! validateBid auction bid
               do! r.SaveBid bid
               return BidAccepted bid
            }
        | Some _ -> Error(BidAlreadyExists bid.id))
    }



