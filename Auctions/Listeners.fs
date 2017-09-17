module Auctions.Listeners
open Commands

let listenTo (r : IRepository) command = 
  match command with
  | BidRetracted bid-> r.SaveBid bid
  | BidAccepted bid-> r.SaveBid bid
  | AuctionAdded auction -> r.SaveAuction auction
  | NoCommand -> Ok ()
