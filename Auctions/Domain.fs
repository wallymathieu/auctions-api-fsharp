module Auctions.Domain

open System
open FSharpPlus

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

module Currency=
  let tryParse c : Currency option= tryParse c |> Option.map Currency
  let VAC = Currency CurrencyCode.VAC
  let value (Currency c) = int64(LanguagePrimitives.EnumToValue c)
  let inline ofValue v = v |> int |> LanguagePrimitives.EnumOfValue<int,CurrencyCode> |> Currency

[<Struct>]
type UserId = UserId of string
with
  override this.ToString()=match this with UserId uId->uId

type User =
  | BuyerOrSeller of id : UserId * name : string
  | Support of id : UserId

  override this.ToString() =
    match this with
    | BuyerOrSeller(id, name) -> $"BuyerOrSeller|{id}|%s{name}"
    | Support(id) -> $"Support|{id}"

  static member getId user =
    match user with
    | BuyerOrSeller(id, _) -> id
    | Support id -> id

  static member Regex = System.Text.RegularExpressions.Regex("(?<type>\w*)\|(?<id>[^|]*)(\|(?<name>.*))?")
  static member TryParse user =
    let m = User.Regex.Match(user)
    if m.Success then
      match (m.Groups["type"].Value, m.Groups["id"].Value, m.Groups["name"].Value) with
      | "BuyerOrSeller", id, name -> Some (BuyerOrSeller(UserId id, name))
      | "Support", id, _ -> Some (Support(UserId id))
      | _, _, _ -> None
    else None
  static member Parse user : User= tryParse user |> Option.defaultWith (fun ()-> failwithf $"Unable to parse %s{user}")

[<Struct>]
type AuctionId = AuctionId of int64
with
  override this.ToString()=match this with AuctionId aId->string aId
//Module AuctionId
  static member unwrap (AuctionId aId)=aId

let (|Currency|_|) = Currency.tryParse

type AmountValue = int64
module AmountValue=
  let zero:AmountValue = 0L
type Amount =
  { value : AmountValue
    currency : Currency }
  override this.ToString() =
    $"{this.currency}%i{this.value}"
  static member Regex = System.Text.RegularExpressions.Regex("(?<currency>[A-Z]+)(?<value>[0-9]+)")
  static member TryParse amount =
    let m = Amount.Regex.Match(amount)
    if m.Success then
      match (m.Groups["currency"].Value, m.Groups["value"].Value) with
      | Currency c, amount -> Some { currency=c; value=AmountValue.Parse amount }
      | _, _ -> None
    else None
  static member Parse amount : Amount= tryParse amount |> Option.defaultWith (fun ()-> failwithf $"Unable to parse %s{amount}")
  static member (+) (a1 : Amount, a2 : Amount) =
      if a1.currency <> a2.currency then failwith "not defined for two different currencies"
      { a1 with value = a1.value + a2.value }
  static member (-) (a1 : Amount, a2 : Amount) =
      if a1.currency <> a2.currency then failwith "not defined for two different currencies"
      { a1 with value = a1.value - a2.value }

module Amount =
  let currency (a:Amount) = a.currency
  let value (a:Amount) = a.value
  let zero c= { currency=c ; value=AmountValue.zero }

let (|Amount|_|) : string -> Amount option = tryParse
let (|AmountValue|_|) : string -> AmountValue option = tryParse
let (|Int64|_|) : string -> int64 option = tryParse

module Timed =
  let at now a = (now, a)

[<AutoOpen>]
module Auctions=
  type TimedAscendingOptions = {
      /// the seller has set a minimum sale price in advance (the 'reserve' price)
      /// and the final bid does not reach that price the item remains unsold
      /// If the reserve price is 0, that is the equivalent of not setting it.
      reservePrice: AmountValue
      /// Sometimes the auctioneer sets a minimum amount by which the next bid must exceed the current highest bid.
      /// Having min raise equal to 0 is the equivalent of not setting it.
      minRaise: AmountValue
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

/// Type of auction
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
    | TimedAscending english -> $"English|{english.reservePrice}|{english.minRaise}|%d{english.timeFrame.Ticks}"
    | SingleSealedBid Blind -> "Blind"
    | SingleSealedBid Vickrey -> "Vickrey"
  static member TryParse typ =
    if String.IsNullOrEmpty typ then
      None
    else
      match (typ.Split('|') |> Seq.toList) with
      | "English"::AmountValue reservePrice::AmountValue minRaise:: [ (Int64 timeframe) ] ->
         Some (TimedAscending {
                reservePrice=reservePrice
                minRaise=minRaise
                timeFrame=TimeSpan.FromTicks(timeframe) })
      | ["Blind"] -> Some (SingleSealedBid Blind)
      | ["Vickrey"] -> Some (SingleSealedBid Vickrey)
      | _ -> None
  static member Parse typ : Type = tryParse typ |> Option.defaultWith (fun ()-> failwithf $"Unable to parse %s{typ}")

type Auction =
  { id : AuctionId
    startsAt : DateTime
    title : string
    /// initial expiry
    expiry : DateTime
    user : User
    typ : Type
    currency:Currency
    openBidders : bool
  }

type Bid =
  { auction : AuctionId
    user : User
    amount : AmountValue
    at : DateTime
  }

[<RequireQualifiedAccess>]
module Bid=
  let amount (bid : Bid) = bid.amount
  let auction (bid : Bid) = bid.auction
  let bidder (bid: Bid) = bid.user
  let at (bid: Bid) = bid.at

type Errors =
  | UnknownAuction of AuctionId
  | AuctionAlreadyExists of AuctionId
  | AuctionHasEnded of AuctionId
  | AuctionHasNotStarted of AuctionId
  | AuctionNotFound of AuctionId
  | SellerCannotPlaceBids of UserId * AuctionId
  | InvalidUserData of String
  | MustPlaceBidOverHighestBid of AmountValue
  | AlreadyPlacedBid

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
  let biddersAreOpen (auction : Auction) = auction.openBidders

  let (|ValidBid|InvalidBid|) (auction : Auction, bid : Bid) =
    if bid.user = auction.user then InvalidBid(SellerCannotPlaceBids(User.getId bid.user, auction.id))
    else if bid.at < auction.startsAt then InvalidBid(Errors.AuctionHasNotStarted auction.id)
    else if bid.at > auction.expiry then InvalidBid(Errors.AuctionHasEnded auction.id)
    else ValidBid

  let validateBid auction bid =
    match (auction,bid) with
    | ValidBid -> Ok()
    | InvalidBid err -> Error err

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
    abstract TryGetAmountAndWinner: unit -> (AmountValue * User) option
    /// returns true if state has ended
    abstract HasEnded: unit -> bool

  type TimedAscendingState =
     | AwaitingStart of start: DateTime * expiry: DateTime * opt:TimedAscendingOptions
     | OnGoing of bids: Bid list * expiry: DateTime * opt:TimedAscendingOptions
     | HasEnded of bids: Bid list * expired: DateTime * opt:TimedAscendingOptions
  with
    member state.Inc now =
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
        | HasEnded _ as ended ->
            ended
    member state.AddBid b=
        let next = state.Inc b.at
        match next with
        | AwaitingStart _ as awaitingStart->
          awaitingStart, Error (AuctionHasNotStarted b.auction)
        | OnGoing (bids,expiry,opt) as ongoing->
          match bids with
          | [] -> OnGoing (b::bids, max expiry (b.at+opt.timeFrame), opt), Ok()
          | highestBid::_ ->
            // you cannot bid lower than the "current bid"
            if b.amount > (highestBid.amount + opt.minRaise)
            then
              OnGoing (b::bids, max expiry (b.at+opt.timeFrame), opt),Ok()
            else
              ongoing, Error (MustPlaceBidOverHighestBid highestBid.amount)
        | HasEnded _ as ended ->
            ended,Error (AuctionHasEnded b.auction)
    interface IState with
      member state.Inc now = state.Inc now
      member state.AddBid b = let next,err= state.AddBid b in (next:>IState, err)
      member state.GetBids () =
        match state with
        | OnGoing(bids,_,_)->bids
        | HasEnded(bids,_,_)->bids
        | AwaitingStart _ ->[]
      member state.TryGetAmountAndWinner () =
        match state with
        | HasEnded (bid::_ ,_ , opt) when opt.reservePrice<=bid.amount -> Some (bid.amount, bid.user)
        | _ -> None

      member state.HasEnded () =match state with | HasEnded _ -> true | _ -> false

  type SingleSealedBidState =
     | AcceptingBids of bids: Map<UserId, Bid> * expiry: DateTime * opt:SingleSealedBidOptions
     | DisclosingBids of bids: Bid list * expired: DateTime * opt:SingleSealedBidOptions
  with
    member state.Inc now =
      match state with
      | AcceptingBids (bids,expiry, opt) as acceptingBids->
        match now>=expiry with
        | false -> acceptingBids
        | true ->
          let bids=bids|>Map.toList|>List.map snd |> List.sortByDescending Bid.amount
          DisclosingBids(bids, expiry, opt)
      | DisclosingBids _ as disclosingBids-> disclosingBids
    member state.AddBid b =
      let next = state.Inc b.at
      match next with
      | AcceptingBids (bids,expiry, opt) as acceptingBids->
        let u=User.getId b.user
        match bids.ContainsKey u with
        | false -> AcceptingBids (bids.Add (u,b), expiry, opt), Ok()
        | true -> acceptingBids, Error AlreadyPlacedBid
      | DisclosingBids _ as disclosingBids->
        disclosingBids, Error (AuctionHasEnded b.auction)

    interface IState with
      member state.Inc now = state.Inc now

      member state.AddBid b = let next, err = state.AddBid b in (next :> IState, err)

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

module Command =
  /// the time when the command was issued
  let getAt =
    function
    | AddAuction(at, _) -> at
    | PlaceBid(at, _) -> at

  let getAuction =
    function
    | AddAuction(_,auction) -> auction.id
    | PlaceBid(_,bid) -> bid.auction

  /// Fold to map of auction that contains both the auction and state
  let foldToMap commands =
    let folder auctions =
          function
          | AddAuction (at,auction) ->
            if auction.expiry > at && not (Map.containsKey auction.id auctions) then
              let empty =Auction.emptyState auction
              Map.add auction.id (auction,empty) auctions
            else
              auctions
          | PlaceBid (_,b) ->
            match Map.tryFind b.auction auctions with
            | Some (auction,state) ->
              match (auction,b) with
              | Auction.ValidBid ->
                let next,_= S.addBid b state
                Map.add auction.id (auction, next) auctions
              | Auction.InvalidBid _ -> auctions
            | None -> auctions
    (Map.empty, commands) ||> List.fold folder
type Event =
  | AuctionAdded of DateTime * Auction
  | BidAccepted of DateTime * Bid

module Event =
  /// the time when the event was sent
  let getAt event =
    match event with
    | AuctionAdded(at, _) -> at
    | BidAccepted(at, _) -> at

  let getAuction event =
    match event with
    | AuctionAdded(_,auction) -> auction.id
    | BidAccepted(_,bid) -> bid.auction

  /// Fold to map of auction that contains both the auction and state
  let foldToMap events =
    let folder auctions =
          function
          | AuctionAdded (_, auction)->
            let empty =Auction.emptyState auction
            Map.add auction.id (auction,empty) auctions
          | BidAccepted (_, b)->
              match Map.tryFind b.auction auctions with
              | Some (auction,state) ->
                let next, _ = S.addBid b state
                Map.add auction.id (auction, next) auctions
              | None -> failwith "could not find auction"
    (Map.empty, events) ||> List.fold folder
type Observable =
  | Commands of Command list
  | Results of Result<Event, Errors> list

// ====================================================================================================
// JSON, not part of the domain but used for serialization
// ====================================================================================================

open Fleece
open Fleece.FSharpData

type Currency with
  static member OfJson json = Currency.tryParse <!> ofJson json >>= (Option.toResultWith <|
                                                                      InvalidValue(typeof<Currency>, json, "Unable to interpret as currency"))
  static member ToJson (x: Currency) = toJson (string x)

type User with
  static member OfJson json : Result<User,_> = tryParse <!> ofJson json >>= (Option.toResultWith <|
                                                                 InvalidValue(typeof<Currency>, json, "Unable to interpret as user"))
  static member ToJson (x: User) = toJson (string x)

type AuctionId with
  static member OfJson json =AuctionId <!> ofJson json
  static member ToJson (AuctionId aId) = toJson aId

type Amount with
  static member OfJson json = Amount.TryParse <!> ofJson json >>= (Option.toResultWith <|
                                                                   InvalidValue(typeof<Currency>, json, "Unable to interpret as amount"))
  static member ToJson (x: Amount) = toJson (string x)

type Type with
  static member OfJson json : Result<Type,_> = tryParse <!> ofJson json >>= (Option.toResultWith <|
                                                                 InvalidValue(typeof<Currency>, json, "unrecognized type"))
  static member ToJson (x: Type) = toJson (string x)
type Errors with
  static member ToJson (x: Errors) =
    match x with
    | UnknownAuction a-> jobj [ "type".="UnknownAuction"; "auctionId" .= a] //NOTE: Duplicate
    | AuctionAlreadyExists a-> jobj [ "type".="AuctionAlreadyExists"; "auctionId" .= a]
    | AuctionHasEnded a-> jobj [ "type".="AuctionHasEnded"; "auctionId" .= a]
    | AuctionHasNotStarted a-> jobj [ "type".="AuctionHasNotStarted"; "auctionId" .= a]
    | AuctionNotFound b-> jobj [ "type".="AuctionNotFound"; "bidId" .= b]
    | SellerCannotPlaceBids (u,a)-> jobj [ "type".="SellerCannotPlaceBids"; "userId" .= string u; "auctionId" .=a]
    | InvalidUserData u-> jobj [ "type".="InvalidUserData"; "user" .= string u]
    | MustPlaceBidOverHighestBid a-> jobj [ "type".="MustPlaceBidOverHighestBid"; "amount" .= a]
    | AlreadyPlacedBid -> jobj [ "type".="AlreadyPlacedBid"]

type Auction with
  static member JsonObjCodec = codec {
    let! id = jreq "id" (fun x -> Some x.id)
    and! startsAt = jreq "startsAt" (fun x -> Some x.startsAt)
    and! title = jreq "title" (fun x -> Some x.title)
    and! expiry = jreq "expiry" (fun x -> Some x.expiry)
    and! user = jreq "user" (fun x -> Some x.user)
    and! typ = jreq "type" (fun x -> Some x.typ)
    and! currency = jreq "currency" (fun x -> Some x.currency)
    and! openBidders = jopt "open" (fun x -> Some x.openBidders)
    return { id = id
             startsAt = startsAt
             title = title
             expiry = expiry
             user = user
             typ = typ
             currency = currency
             openBidders = Option.defaultValue false openBidders } }
type Bid with
  static member JsonObjCodec = codec {
    let! auction = jreq "auction" (fun x -> Some x.auction)
    and! user = jreq "user" (fun x -> Some x.user)
    and! amount = jreq "amount" (fun x -> Some x.amount)
    and! at = jreq "at" (fun x -> Some x.at)
    return{ auction =auction; user = user; amount=amount; at=at } }
/// tag Json codec with property and value inside the Json encoded by the codec
let inline tag prop value codec =
    let matchPropValue (o:PropertyList<_>) =
         match o[prop] with
         | a::_ when (ofJson a) = Ok value -> Ok o
         | a::_ -> Decode.Fail.invalidValue a value
         | [] -> Decode.Fail.propertyNotFound prop o
    codec
    |> Codec.compose (
      matchPropValue
      <->
      fun encoded ->
        if Seq.isEmpty encoded then encoded // we have not encoded anything so no need to tag it
        else PropertyList (List.toArray ((toList encoded) @ [prop, toJson value])) )

type Command with
  static member JsonObjCodec =
    let auctionCodec =
      let create d a = AddAuction (d,a)
      create
      <!> jreq "at" (function AddAuction (d,_) -> Some d | _ -> None)
      <*> jreq "auction" (function AddAuction (_,a) -> Some a | _ -> None)
    let bidCodec =
      let create d b = PlaceBid (d,b)
      create
      <!> jreq "at" (function PlaceBid (d,_)-> Some d | _ -> None)
      <*> jreq "bid" (function PlaceBid (_,a)-> Some a | _ -> None)
    tag "$type" "AddAuction" auctionCodec <|> tag "$type" "PlaceBid" bidCodec

type Event with
  static member JsonObjCodec =
    let auctionCodec =
      let create d a = AuctionAdded (d,a)
      create
      <!> jreq "at" (function AuctionAdded (d,_)-> Some d | _ -> None)
      <*> jreq "auction" (function AuctionAdded (_,a)-> Some a | _ -> None)
    let bidCodec =
      let create d b = BidAccepted (d,b)
      create
      <!> jreq "at" (function BidAccepted (d,_)-> Some d | _ -> None)
      <*> jreq "bid" (function BidAccepted (_,a)-> Some a | _ -> None)
    (tag "$type" "AuctionAdded" auctionCodec) <|> (tag "$type" "BidAccepted" bidCodec)

type Observable with
  static member ToJson (x:Observable) =
    match x with
    | Commands commands->
      jobj [ "$type" .= "Commands"; "commands" .= commands]
    | Results results->
      let mapToJson = function | Ok o-> JArray [|JString "Ok"; toJson  o |] | Error e->JArray [JString "Error";toJson e]
      let jArray = results |> List.map mapToJson |> List.toArray |> JArray
      jobj [ "$type" .= "Results"; "results", jArray]
