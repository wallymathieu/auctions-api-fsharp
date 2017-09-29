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

/// the intention of this function is to return the modified repository and a success, or return an error
/// I.e. this function does not know if it's an immutable repository 
let handleCommand (r : IRepository) command : Result<IRepository*CommandSuccess,Errors> = 
  match command with
  | Empty(at = at) -> Ok(r, NoCommand )
  | AddAuction (auction=auction) -> 
    let id = auction.id
    either { 
      match r.TryGetAuction id with
      | Some _ -> return! Error(AuctionAlreadyExists id)
      | None -> 
        yield! r.SaveAuction auction
        yield AuctionAdded auction
    }
  | PlaceBid (bid) -> 
    either { 
      let! auction = r.GetAuction bid.auction
      return! (
        match r.TryGetBid bid.id with
        | None -> 
            either{
               do! validateBid auction bid
               yield! r.SaveBid bid
               yield BidAccepted bid
            }
        | Some _ -> Error(BidAlreadyExists bid.id))
    }



