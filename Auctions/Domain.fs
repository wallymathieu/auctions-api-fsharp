module Auctions.Domain

open System
open System.ComponentModel
open FSharp.Data
open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators

module Result=
  let ofOption error option = match option with | Some v -> Ok v | None -> Error error

type CurrencyCode =
  /// virtual auction currency
  |VAC=1001
  /// Swedish 'Krona'
  |SEK=752
  /// Danish 'Krone'
  |DKK=208
[<Struct>]
type Currency = Currency of CurrencyCode
with
  override x.ToString () =
    let unwrap (Currency c) = c
    Enum.GetName (typeof<CurrencyCode>, unwrap x)

  static member OfJson json = Currency.tryParse <!> ofJson json >>= (Result.ofOption "Invalid currency")
  static member ToJson (x: Currency) = toJson (string x)
//module Currency=
  static member tryParse c : Currency option= tryParse c |> Option.map Currency
  static member VAC = Currency CurrencyCode.VAC
  static member value (Currency c) = int64(LanguagePrimitives.EnumToValue c)
  static member inline ofValue (v) = v |> int |> LanguagePrimitives.EnumOfValue<int,CurrencyCode> |> Currency

[<Struct>]
type UserId = UserId of string
with
  override this.ToString()=match this with UserId uId->uId

type User =
  | BuyerOrSeller of id : UserId * name : string
  | Support of id : UserId

  override this.ToString() =
    match this with
    | BuyerOrSeller(id, name) -> sprintf "BuyerOrSeller|%O|%s" id name
    | Support(id) -> sprintf "Support|%O" id

  static member getId user =
    match user with
    | BuyerOrSeller(id, _) -> id
    | Support id -> id

  static member Regex = System.Text.RegularExpressions.Regex("(?<type>\w*)\|(?<id>[^|]*)(\|(?<name>.*))?")
  static member tryParse user =
    let m = User.Regex.Match(user)
    if m.Success then
      match (m.Groups.["type"].Value, m.Groups.["id"].Value, m.Groups.["name"].Value) with
      | "BuyerOrSeller", id, name -> Some (BuyerOrSeller(UserId id, name))
      | "Support", id, _ -> Some (Support(UserId id))
      | type', _, _ -> None
    else None
  static member __parse user = User.tryParse user |> Option.defaultWith (fun ()-> failwithf "Unable to parse %s" user)
  static member OfJson json = User.tryParse <!> ofJson json >>= (Result.ofOption "Invalid user")
  static member ToJson (x: User) = toJson (string x)

[<Struct>]
type BidId = BidId of Guid
with
  override this.ToString()=match this with BidId bId->bId.ToString("N")
  static member OfJson json = BidId.tryParse <!> ofJson json >>= (Result.ofOption "Invalid bid id")
  static member ToJson (b:BidId) = toJson (string b)
  static member New()= Guid.NewGuid() |> BidId
//Module BidId
  static member tryParse v : BidId option= tryParse v |> Option.map BidId
  static member unwrap (BidId bId)=bId

[<Struct>]
type AuctionId = AuctionId of int64
with
  override this.ToString()=match this with AuctionId aId->string aId
  static member OfJson json =AuctionId <!> ofJson json
  static member ToJson (b:AuctionId) = toJson (string b)
//Module AuctionId
  static member unwrap (AuctionId aId)=aId

let (|Currency|_|) = Currency.tryParse

type Amount =
  { value : int64
    currency : Currency }
  override this.ToString() =
    sprintf "%O%i" this.currency this.value
  static member Regex = System.Text.RegularExpressions.Regex("(?<currency>[A-Z]+)(?<value>[0-9]+)")
  static member tryParse amount =
    let m = Amount.Regex.Match(amount)
    if m.Success then
      match (m.Groups.["currency"].Value, m.Groups.["value"].Value) with
      | Currency c, amount -> Some { currency=c; value=Int64.Parse amount }
      | type', _ -> None
    else None
  static member __parse amount = Amount.tryParse amount |> Option.defaultWith (fun ()-> failwithf "Unable to parse %s" amount)
  static member (+) (a1 : Amount, a2 : Amount) =
      if a1.currency <> a2.currency then failwith "not defined for two different currencies"
      { a1 with value = a1.value + a2.value }
  static member (-) (a1 : Amount, a2 : Amount) =
      if a1.currency <> a2.currency then failwith "not defined for two different currencies"
      { a1 with value = a1.value - a2.value }
  static member OfJson json = Amount.tryParse <!> ofJson json >>= (Result.ofOption "Invalid amount")
  static member ToJson (x: Amount) = toJson (string x)

module Amount=
  let currency (a:Amount) = a.currency
  let value (a:Amount) = a.value
  let zero c= { currency=c ; value=0L}

let (|Amount|_|) = Amount.tryParse
let (|Int64|_|) = tryParse

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
      | TimedAscending english -> sprintf "English|%O|%O|%d"
                                    (english.reservePrice) (english.minRaise) english.timeFrame.Ticks
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
    static member __parse typ = Type.tryParse typ |> Option.defaultWith (fun ()-> failwithf "Unable to parse %s" typ)
    static member OfJson json = Type.tryParse <!> ofJson json >>= (Result.ofOption "Invalid auction type")
    static member ToJson (x:Type) = toJson (string x)

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
with
  static member JsonObjCodec =
    fun id startsAt title expiry user typ currency -> { id =id; startsAt =startsAt; title = title; expiry=expiry; user=user; typ=typ; currency=currency }
    |> withFields
    |> jfield "id"      (fun x -> x.id)
    |> jfield "startsAt" (fun x -> x.startsAt)
    |> jfield "title"    (fun x -> x.title)
    |> jfield "expiry"  (fun x -> x.expiry)
    |> jfield "user"      (fun x -> x.user)
    |> jfield "type"      (fun x -> x.typ)
    |> jfield "currency"  (fun x -> x.currency)

type Bid =
  { id : BidId
    auction : AuctionId
    user : User
    amount : Amount
    at : DateTime
  }
with
  static member JsonObjCodec =
    fun id auction user amount at-> { id =id; auction =auction; user = user; amount=amount; at=at }
    |> withFields
    |> jfield "id"      (fun x -> x.id)
    |> jfield "auction" (fun x -> x.auction)
    |> jfield "user"    (fun x -> x.user)
    |> jfield "amount"  (fun x -> x.amount)
    |> jfield "at"      (fun x -> x.at)

[<RequireQualifiedAccess>]
module Bid=
  let getId (bid : Bid) = bid.id
  let amount (bid : Bid) = bid.amount
  let auction (bid : Bid) = bid.auction
  let bidder (bid: Bid) = bid.user

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
  static member ToJson (x: Errors) =
    match x with
    | UnknownAuction a-> jobj [ "type".="UnknownAuction"; "auctionId" .= a] //NOTE: Duplicate
    | UnknownBid b-> jobj [ "type".="UnknownBid"; "bidId" .= b]
    | BidAlreadyExists b-> jobj [ "type".="BidAlreadyExists"; "bidId" .= b]
    | AuctionAlreadyExists a-> jobj [ "type".="AuctionAlreadyExists"; "auctionId" .= a]
    | AuctionHasEnded a-> jobj [ "type".="AuctionHasEnded"; "auctionId" .= a]
    | AuctionHasNotStarted a-> jobj [ "type".="AuctionHasNotStarted"; "auctionId" .= a]
    | AuctionNotFound b-> jobj [ "type".="AuctionNotFound"; "bidId" .= b]
    | SellerCannotPlaceBids (u,a)-> jobj [ "type".="SellerCannotPlaceBids"; "userId" .= string u; "auctionId" .=a]
    | BidCurrencyConversion (b,c)-> jobj [ "type".="BidCurrencyConversion"; "bidId" .= b; "currency" .= c]
    | InvalidUserData u-> jobj [ "type".="InvalidUserData"; "user" .= u]
    | MustPlaceBidOverHighestBid a-> jobj [ "type".="MustPlaceBidOverHighestBid"; "amount" .= a]
    | AlreadyPlacedBid -> jobj [ "type".="AlreadyPlacedBid"]

[<RequireQualifiedAccess>]
module Auction=
  let getId (auction : Auction) = auction.id
  let title (auction : Auction) = auction.title
  let expiry (auction : Auction) = auction.expiry
  let startsAt (auction : Auction) = auction.startsAt
  let typ (auction : Auction) = auction.typ
  let currency (auction : Auction) = auction.currency
  let user (auction : Auction) = auction.user
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
    interface IState with
      member state.Inc now =
        match state with
        | AwaitingStart (start,expiry, opt) as awaitingStart->
          match (now>start, now<expiry) with
          | true,true->
            OnGoing([],expiry, opt) :> IState
          | true,false->
            HasEnded([],expiry, opt) :> IState
          | false, _ ->
            awaitingStart :> IState
        | OnGoing (bids,expiry,opt) as ongoing->
          if now<expiry then
            ongoing :> IState
          else
            HasEnded (bids, expiry, opt) :> IState
        | HasEnded _ as ended ->
            ended :> IState

      member state.AddBid b = // note that this is increment + mutate in one operation
        match state with
        | AwaitingStart (start,expiry, opt) as awaitingStart->
          match (b.at>start, b.at<expiry) with
          | true,true->
            OnGoing([b], max expiry (b.at+opt.timeFrame), opt) :>IState,Ok()
          | true,false->
            HasEnded([],expiry, opt ):>IState,Error (AuctionHasEnded b.auction)
          | false, _ ->
            awaitingStart :>IState,Error (AuctionHasNotStarted b.auction)
        | OnGoing (bids,expiry,opt) as ongoing->
          if b.at<expiry then
            match bids with
            | [] -> OnGoing (b::bids, max expiry (b.at+opt.timeFrame), opt):>IState,Ok()
            | highestBid::_ ->
              // you cannot bid lower than the "current bid"
              if b.amount > (highestBid.amount + opt.minRaise)
              then
                OnGoing (b::bids, max expiry (b.at+opt.timeFrame), opt):>IState,Ok()
              else
                ongoing:>IState, Error (MustPlaceBidOverHighestBid highestBid.amount)
          else
            HasEnded (bids, expiry, opt):>IState, Error (AuctionHasEnded b.auction)
        | HasEnded _ as ended ->
            ended:>IState,Error (AuctionHasEnded b.auction)
      member state.GetBids () =
        match state with
        | OnGoing(bids,_,_)->bids
        | HasEnded(bids,_,_)->bids
        | AwaitingStart _ ->[]
      member state.TryGetAmountAndWinner () =
        match state with
        | HasEnded (bid::_ ,_ , opt) when opt.reservePrice<bid.amount -> Some (bid.amount, bid.user)
        | _ -> None

      member state.HasEnded () =match state with | HasEnded _ -> true | _ -> false

  type SingleSealedBidState =
     | AcceptingBids of bids: Map<UserId, Bid> * expiry: DateTime * opt:SingleSealedBidOptions
     | DisclosingBids of bids: Bid list * expired: DateTime * opt:SingleSealedBidOptions
  with

    interface IState with
      member state.Inc now =
        match state with
        | AcceptingBids (bids,expiry, opt) as acceptingBids->
          match now>=expiry with
          | false -> acceptingBids :>IState
          | true ->
            let bids=bids|>Map.toList|>List.map snd |> List.sortByDescending Bid.amount
            DisclosingBids(bids, expiry, opt):>IState
        | DisclosingBids _ as disclosingBids-> disclosingBids:>IState

      member state.AddBid b =
        match state with
        | AcceptingBids (bids,expiry, opt) as acceptingBids->
          let u=User.getId b.user
          match b.at>=expiry, bids.ContainsKey (u) with
          | false, false -> AcceptingBids (bids.Add (u,b), expiry, opt):>IState, Ok()
          | _, true -> acceptingBids:>IState, Error AlreadyPlacedBid
          | true,_ ->
            let bids=bids|>Map.toList|>List.map snd |> List.sortByDescending Bid.amount
            DisclosingBids(bids,expiry, opt):>IState, Error (AuctionHasEnded b.auction)
        | DisclosingBids _ as disclosingBids->
          disclosingBids:>IState, Error (AuctionHasEnded b.auction)

      member state.GetBids () =
        match state with
        | DisclosingBids (bids, _, _)->bids
        | AcceptingBids _-> []

      member state.TryGetAmountAndWinner () =
        match state with
        | DisclosingBids (highestBid :: secondHighest :: _, _, Vickrey) ->
          Some (secondHighest.amount,highestBid.user)
        | DisclosingBids ([highestBid], _, Vickrey) ->
          Some (highestBid.amount,highestBid.user)
        | DisclosingBids (highestBid :: _, _, Blind) ->
          Some (highestBid.amount,highestBid.user)
        | _  -> None

      member state.HasEnded () =
        match state with | DisclosingBids _ -> true | _ -> false

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
  static member OfJson json =
    let create id startsAt title expiry user typ currency= { id =id; startsAt =startsAt; title = title; expiry=expiry; user=user; typ=typ; currency=currency }
    match json with
    | JObject o -> monad {
        let! t = o .@ "$type"
        match t with
        | "AddAuction" ->
          let create d a = AddAuction (d,a)
          return! (create <!> (o .@ "at") <*> (o .@ "auction"))
        | "PlaceBid" ->
          let create d b = PlaceBid (d,b)
          return! (create <!> (o .@ "at") <*> (o .@ "bid"))
        | x -> return! (Error (sprintf "Expected known $type, found %A" x))
      }
    | x -> Error (sprintf "Expected Auction, found %A" x)
  static member ToJson (x: Command) =
    match x with
    | AddAuction (d,a)-> jobj [ "$type" .= "AddAuction"; "at" .= d; "auction" .= a]
    | PlaceBid (d,b)-> jobj [ "$type" .= "PlaceBid"; "at" .= d; "bid" .= b]
type CommandSuccess =
  | AuctionAdded of DateTime * Auction
  | BidAccepted of DateTime * Bid
  static member ToJson (x: CommandSuccess) =
    match x with
    | AuctionAdded (d,a)-> jobj [ "$type" .= "AuctionAdded"; "at" .= d; "auction" .= a]
    | BidAccepted (d,b)-> jobj [ "$type" .= "BidAccepted"; "at" .= d; "bid" .= b]


type Observable =
  | Commands of Command list
  | Results of Result<CommandSuccess, Errors> list
  static member ToJson (x: Observable) =
    match x with
    | Commands commands->
      jobj [ "$type" .= "Commands"; "commands" .= commands]
    | Results results->
      let mapToJson = function | Ok o-> JArray [|JString "Ok"; toJson o|] | Error e->JArray [JString "Error";toJson e]
      let jresults = results |> List.map mapToJson |> List.toArray |> JArray
      jobj [ "$type" .= "Results"; "results", jresults]
