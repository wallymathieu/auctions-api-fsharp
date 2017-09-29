module Auctions.Domain

open System

type Currency = string

type UserId = Guid

type User = 
  | BuyerOrSeller of id : UserId * name : string
  | Support of id : UserId
  
  override this.ToString() = 
    match this with
    | BuyerOrSeller(id, name) -> sprintf "BuyerOrSeller|%s|%s" (id.ToString("N")) name
    | Support(id) -> sprintf "Support|%s" (id.ToString("N"))
  
  static member getId user = 
    match user with
    | BuyerOrSeller(id, _) -> id
    | Support id -> id
  
  static member parse user = 
    let userRegex = System.Text.RegularExpressions.Regex("(?<type>\w*)\|(?<id>[^|]*)(\|(?<name>.*))?")
    let m = userRegex.Match(user)
    if m.Success then 
      match (m.Groups.["type"].Value, m.Groups.["id"].Value, m.Groups.["name"].Value) with
      | "BuyerOrSeller", id, name -> BuyerOrSeller(id |> Guid.Parse, name)
      | "Support", id, _ -> Support(id |> Guid.Parse)
      | type', _, _ -> failwithf "Unknown type of user %s" type'
    else failwithf "Could not parse %s" user

type BidId = Guid

type AuctionId = int64

type Amount = 
  { value : float
    currency : Currency }

type Auction = 
  { id : AuctionId
    startsAt: DateTime
    title : string
    endsAt : DateTime
    user : User }
  static member getId (auction : Auction) = auction.id

type Bid = 
  { id : BidId
    auction : AuctionId 
    at : DateTime
    user : User
    amount : Amount}
  static member getId (bid : Bid) = bid.id

type Errors = 
  | UnknownAuction of AuctionId
  | UnknownBid of BidId
  | BidAlreadyExists of BidId
  | AuctionAlreadyExists of AuctionId
  | AuctionHasEnded of AuctionId
  | SellerCannotPlaceBids of UserId * AuctionId
