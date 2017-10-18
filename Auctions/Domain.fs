module Auctions.Domain

open System
open System.ComponentModel
open Newtonsoft.Json
open Saithe

type Currency = 
  /// virtual acution currency
  |VAC=1001  
  /// Swedish 'Krona'
  |SEK=752
  /// Danish 'Krone'
  |DKK=208

type UserId = string

[<TypeConverter(typeof<ParseTypeConverter<User>>)>]
[<JsonConverter(typeof<ParseTypeJsonConverter<User>>)>]
type User = 
  | BuyerOrSeller of id : UserId * name : string
  | Support of id : UserId
  
  override this.ToString() = 
    match this with
    | BuyerOrSeller(id, name) -> sprintf "BuyerOrSeller|%s|%s" (id) name
    | Support(id) -> sprintf "Support|%s" (id)
  
  static member getId user = 
    match user with
    | BuyerOrSeller(id, _) -> id
    | Support id -> id
  
  static member tryParse user = 
    let userRegex = System.Text.RegularExpressions.Regex("(?<type>\w*)\|(?<id>[^|]*)(\|(?<name>.*))?")
    let m = userRegex.Match(user)
    if m.Success then 
      match (m.Groups.["type"].Value, m.Groups.["id"].Value, m.Groups.["name"].Value) with
      | "BuyerOrSeller", id, name -> Some (BuyerOrSeller(id, name))
      | "Support", id, _ -> Some (Support(id))
      | type', _, _ -> None
    else None
  [<CompiledName("Parse")>]
  static member parse user = 
    match User.tryParse user with
    | Some user->user
    | None -> raise (FormatException "InvalidUser")
    

type BidId = Guid

type AuctionId = int64

[<TypeConverter(typeof<ParseTypeConverter<Amount>>)>]
[<JsonConverter(typeof<ParseTypeJsonConverter<Amount>>)>]
type Amount = 
  { value : float
    currency : Currency }
  override this.ToString() = 
    sprintf "%s%f" (this.currency.ToString()) this.value

  static member tryParse amount = 
    let userRegex = System.Text.RegularExpressions.Regex("(?<currency>[A-Z]+)(?<value>[0-9]+)")
    let m = userRegex.Match(amount)
    if m.Success then 
      match (m.Groups.["currency"].Value, m.Groups.["value"].Value) with
      | "VAC", amount -> Some { currency=Currency.VAC; value=Double.Parse amount }
      | type', _ -> None
    else None
  [<CompiledName("Parse")>]
  static member parse amount = 
    match Amount.tryParse amount with
    | Some amount->amount
    | None -> raise (FormatException "InvalidAmount")

module Timed = 
  let atNow a = (DateTime.UtcNow, a)

type Auction = 
  { id : AuctionId
    startsAt : DateTime
    title : string
    endsAt : DateTime
    user : User }
  static member getId (auction : Auction) = auction.id
  static member hasEnded now (auction : Auction) = auction.endsAt>now

type Bid = 
  { id : BidId
    auction : AuctionId
    user : User
    amount : Amount
    at : DateTime }
  static member getId (bid : Bid) = bid.id
  static member getAmount (bid : Bid) = bid.amount

type Errors = 
  | UnknownAuction of AuctionId
  | UnknownBid of BidId
  | BidAlreadyExists of BidId
  | AuctionAlreadyExists of AuctionId
  | AuctionHasEnded of AuctionId
  | AuctionNotFound of AuctionId
  | SellerCannotPlaceBids of UserId * AuctionId
  | BidCurrencyConversion of BidId * Currency
  | InvalidUserData of string