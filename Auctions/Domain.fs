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

module Currency=
  let tryParse c : Currency option= Parse.toTryParse Currency.TryParse c

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
let (|Currency|_|) = Currency.tryParse

[<TypeConverter(typeof<ParseTypeConverter<Amount>>)>]
[<JsonConverter(typeof<ParseTypeJsonConverter<Amount>>)>]
type Amount = 
  { value : int64
    currency : Currency }
  override this.ToString() = 
    sprintf "%s%i" (this.currency.ToString()) this.value

  static member tryParse amount = 
    let userRegex = System.Text.RegularExpressions.Regex("(?<currency>[A-Z]+)(?<value>[0-9]+)")
    let m = userRegex.Match(amount)
    if m.Success then 
      match (m.Groups.["currency"].Value, m.Groups.["value"].Value) with
      | Currency c, amount -> Some { currency=c; value=Int64.Parse amount }
      | type', _ -> None
    else None
  [<CompiledName("Parse")>]
  static member parse amount = 
    match Amount.tryParse amount with
    | Some amount->amount
    | None -> raise (FormatException "InvalidAmount")
  static member (+) (a1 : Amount, a2 : Amount) =
      if a1.currency <> a2.currency then failwith "not defined for two different currencies"
      { a1 with value = a1.value + a2.value }
  static member (-) (a1 : Amount, a2 : Amount) = 
      if a1.currency <> a2.currency then failwith "not defined for two different currencies"
      { a1 with value = a1.value - a2.value }
module Amount=
  let zero c= { currency=c ; value=0L}

let (|Amount|_|) = Amount.tryParse
let (|Int64|_|) = Parse.toTryParse Int64.TryParse

module Timed = 
  let atNow a = (DateTime.UtcNow, a)

[<AutoOpen>]
module Auctions=
  type TimedAscendingOptions = { 
      /// the seller has set a minimum sale price in advance (the 'reserve' price) 
      /// and the final bid does not reach that price the item remains unsold
      /// If the reserve price is 0, that is the equivalent of not setting it.
      reservePrice: Amount
      /// Sometimes the auctioneer sets a minimum amount by which the next bid must exceed the current highest bid.
      /// Having min raise equal to 0 is the equivalent of not setting it.
      minRaise: Amount 
      /// If no competing bidder challenges the standing bid within a given time frame, 
      /// the standing bid becomes the winner, and the item is sold to the highest bidder 
      /// at a price equal to his or her bid.
      timeFrame: TimeSpan
    }
  type SingleSealedBidOptions =
    /// Sealed first-price auction 
    /// In this type of auction all bidders simultaneously submit sealed bids so that no bidder knows the bid of any
    /// other participant. The highest bidder pays the price they submitted.
    /// This type of auction is distinct from the English auction, in that bidders can only submit one bid each.
    |Blind
    /// Also known as a sealed-bid second-price auction.
    /// This is identical to the sealed first-price auction except that the winning bidder pays the second-highest bid
    /// rather than his or her own
    |Vickrey

  [<TypeConverter(typeof<ParseTypeConverter<Type>>)>]
  [<JsonConverter(typeof<ParseTypeJsonConverter<Type>>)>]
  type Type=
     /// also known as an open ascending price auction
     /// The auction ends when no participant is willing to bid further
     | TimedAscending of TimedAscendingOptions
     | SingleSealedBid of SingleSealedBidOptions
     // | Swedish : same as english, but bidders are not bound by bids and the seller is free to accept or decline any bid 
     // this is a distinction compared to the English auction with open exit rule
     // | Dutch of DutchOptions : since this presumes a different kind of model 
     // we could model this as first bid ends auction
     // the time of the bid implies the price
    
    with
    override this.ToString() = 
      match this with
      | TimedAscending english -> sprintf "English|%s|%s|%d" 
                                    (english.reservePrice.ToString()) (english.minRaise.ToString()) english.timeFrame.Ticks
      | SingleSealedBid Blind -> sprintf "Blind"
      | SingleSealedBid Vickrey -> sprintf "Vickrey"
    static member tryParse typ =
      if String.IsNullOrEmpty typ then
        None
      else
        match (typ.Split('|') |> Seq.toList) with
        | "English"::(Amount reservePrice)::(Amount minRaise):: (Int64 timeframe) :: [] -> 
           Some (TimedAscending { 
                  reservePrice=reservePrice
                  minRaise=minRaise
                  timeFrame=TimeSpan.FromTicks(timeframe) })
        | ["Blind"] -> Some (SingleSealedBid Blind)
        | ["Vickrey"] -> Some (SingleSealedBid Vickrey)
        | _ -> None
    
    [<CompiledName("Parse")>]
    static member parse typ = 
      match Type.tryParse typ with
      | Some t->t
      | None -> raise (FormatException "Invalid Type")

type Auction = 
  { id : AuctionId
    startsAt : DateTime
    title : string
    /// initial expiry
    expiry : DateTime
    user : User 
    typ : Type
    currency:Currency
  }

type Bid = 
  { id : BidId
    auction : AuctionId
    user : User
    amount : Amount
    at : DateTime
  }
module Bid=
  let getId (bid : Bid) = bid.id
  let getAmount (bid : Bid) = bid.amount
  let getBidder (bid: Bid) = bid.user

type Errors = 
  | UnknownAuction of AuctionId
  | UnknownBid of BidId
  | BidAlreadyExists of BidId
  | AuctionAlreadyExists of AuctionId
  | AuctionHasEnded of AuctionId
  | AuctionHasNotStarted of AuctionId
  | AuctionNotFound of AuctionId
  | SellerCannotPlaceBids of UserId * AuctionId
  | BidCurrencyConversion of BidId * Currency
  | InvalidUserData of string
  | MustPlaceBidOverHighestBid of Amount
  | AlreadyPlacedBid

[<RequireQualifiedAccess>]
module Auction=
  let getId (auction : Auction) = auction.id
  /// if the bidders are open or anonymous
  /// for instance in a 'swedish' type auction you get to know the other bidders as the winner
  let biddersAreOpen (auction : Auction) = true

  let validateBid (bid : Bid) (auction : Auction)= 
    if bid.user = auction.user then Error(SellerCannotPlaceBids(User.getId bid.user, auction.id))
    else if bid.amount.currency <> auction.currency then Error(Errors.BidCurrencyConversion(bid.id, bid.amount.currency))
    else Ok()

[<AutoOpen>]
module State=
  [<Interface>]
  type IState=
    /// increment state based on now
    abstract Inc: DateTime -> IState
    /// increment state and add bid
    abstract AddBid: Bid -> IState*Result<unit ,Errors>
    /// get bids for state (will return empty if in a state that does not disclose bids)
    abstract GetBids: unit -> Bid list
    /// try to get amount and winner, will return None if no winner 
    abstract TryGetAmountAndWinner: unit -> (Amount * User) option
    /// returns true if state has ended
    abstract HasEnded: unit -> bool

  type TimedAscendingState =
     | AwaitingStart of start: DateTime * expiry: DateTime * opt:TimedAscendingOptions
     | OnGoing of bids: Bid list * expiry: DateTime * opt:TimedAscendingOptions
     | HasEnded of bids: Bid list * expired: DateTime * opt:TimedAscendingOptions
  with 
   member state.inc (now) = 
    match state with
    | AwaitingStart (start,expiry, opt) as awaitingStart->
      match (now>start, now<expiry) with 
      | true,true->
        OnGoing([],expiry, opt)
      | true,false->
        HasEnded([],expiry, opt)
      | false, _ ->
        awaitingStart
    | OnGoing (bids,expiry,opt) as ongoing->
      if now<expiry then
        ongoing
      else
        HasEnded (bids, expiry, opt)
    | HasEnded (bids,expired, opt) as ended ->
        ended
    member state.addBid (b:Bid) = // note that this is increment + mutate in one operation
      match state with
      | AwaitingStart (start,expiry, opt) as awaitingStart->
        match (b.at>start, b.at<expiry) with 
        | true,true->
          OnGoing([b], max expiry (b.at+opt.timeFrame), opt),Ok()
        | true,false->
          HasEnded([],expiry, opt),Error (AuctionHasEnded b.auction)
        | false, _ ->
          awaitingStart,Error (AuctionHasNotStarted b.auction)
      | OnGoing (bids,expiry,opt) as ongoing->
        if b.at<expiry then
          match bids with
          | [] -> OnGoing (b::bids, max expiry (b.at+opt.timeFrame), opt),Ok()
          | highestBid::xs -> 
            // you cannot bid lower than the "current bid"
            if b.amount > (highestBid.amount + opt.minRaise)
            then
              OnGoing (b::bids, max expiry (b.at+opt.timeFrame), opt),Ok()
            else 
              ongoing, Error (MustPlaceBidOverHighestBid highestBid.amount)
        else
          HasEnded (bids, expiry, opt), Error (AuctionHasEnded b.auction)
      | HasEnded (bids,expired, opt) as ended ->
          ended,Error (AuctionHasEnded b.auction)
    member state.tryGetAmountAndWinner () =
      match state with
      | HasEnded (bid::rest,expired, opt) when opt.reservePrice<bid.amount -> Some (bid.amount, bid.user)
      | _ -> None
    member state.getBids()=
      match state with
      | OnGoing(bids,_,_)->bids
      | HasEnded(bids,_,_)->bids
      | AwaitingStart _ ->[]
    member state.hasEnded ()=match state with | HasEnded _ -> true | _ -> false

    interface IState with
      member s.Inc now =s.inc now :>IState
      member s.AddBid bid =
        let (state,res) =s.addBid bid
        (state:>IState,res)
      member s.GetBids () = s.getBids()
      member s.TryGetAmountAndWinner () = s.tryGetAmountAndWinner ()
      member s.HasEnded () = s.hasEnded()

  type SingleSealedBidState =
     | AcceptingBids of bids: Map<UserId, Bid> * expiry: DateTime * opt:SingleSealedBidOptions
     | DisclosingBids of bids: Bid list * expired: DateTime * opt:SingleSealedBidOptions
  with 
    member state.inc now = 
      match state with
      | AcceptingBids (bids,expiry, opt) as acceptingBids-> 
        match now>=expiry with
        | false -> AcceptingBids (bids, expiry, opt)
        | true -> 
          let bids=bids|>Map.toList|>List.map snd |> List.sortByDescending Bid.getAmount
          DisclosingBids(bids, expiry, opt)
      | DisclosingBids _ as disclosingBids-> disclosingBids
    member state.addBid (b:Bid) = 
      match state with
      | AcceptingBids (bids,expiry, opt) as acceptingBids-> 
        let u=User.getId b.user
        match b.at>=expiry, bids.ContainsKey (u) with
        | false, false -> AcceptingBids (bids.Add (u,b), expiry, opt), Ok()
        | _, true -> acceptingBids, Error AlreadyPlacedBid 
        | true,_ -> 
          let bids=bids|>Map.toList|>List.map snd
          DisclosingBids(bids,expiry, opt), Error (AuctionHasEnded b.auction)
      | DisclosingBids _ as disclosingBids-> 
        disclosingBids, Error (AuctionHasEnded b.auction)
    member state.tryGetAmountAndWinner () =
      match state with
      | DisclosingBids (highestBid :: secondHighest :: _, expired, Vickrey) ->
        Some (secondHighest.amount,highestBid.user)
      | DisclosingBids (highestBid :: [], expired, Vickrey) ->
        Some (highestBid.amount,highestBid.user)
      | DisclosingBids (highestBid :: _, expired, Blind) ->
        Some (highestBid.amount,highestBid.user)
      | _  -> None
    member state.getBids() =
      match state with
      | DisclosingBids (bids,expired,t)->bids
      | AcceptingBids _-> []
    member state.hasEnded ()=match state with | DisclosingBids _ -> true | _ -> false

    interface IState with
      member s.Inc now =s.inc now :>IState
      member s.AddBid bid =
        let (state,res) =s.addBid bid
        (state:>IState,res)
      member s.GetBids () = s.getBids()
      member s.TryGetAmountAndWinner () = s.tryGetAmountAndWinner ()
      member s.HasEnded () = s.hasEnded()

  type S=IState
  [<RequireQualifiedAccess>]
  module Auction=
    let emptyState (a:Auction) : S= 
      match a.typ with
      | SingleSealedBid opt -> AcceptingBids(Map.empty, a.expiry, opt) :>IState
      | TimedAscending opt -> AwaitingStart(a.startsAt,a.expiry, opt) :>IState

  module S=
    let inline inc (now:DateTime) (state:S) :S= state.Inc now
    let inline addBid (bid:Bid) (state:S)=state.AddBid bid
    let inline getBids (state:S)=state.GetBids ()
    let inline tryGetAmountAndWinner (state:S)=state.TryGetAmountAndWinner ()
    let inline hasEnded (state:S)=state.HasEnded ()

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

