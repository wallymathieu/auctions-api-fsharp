module Auctions.GraphQL
open Auctions.Domain
// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

let CurrencyType =
  let enumName v=System.Enum.GetName(typeof<CurrencyCode>, v)
  let values = System.Linq.Enumerable.Cast<CurrencyCode> ( System.Enum.GetValues(typeof<CurrencyCode>))
  Define.Enum<CurrencyCode>(
    name = "Currency",
    description = "Currency",
    options= (values |> Seq.map (fun v -> Define.EnumValue(enumName v,v) ) |> Seq.toList)
    )

let AmountType =
  let currencyCode (a:Amount) = Currency.code (a.currency)
  Define.Object<Amount>(
    name = "Amount",
    description = "Amount",
    isTypeOf = (fun o -> o :? Amount),
    fieldsFn = fun () ->
    [
        Define.Field("value", String, "Value of amount.", fun _ h -> string h.value)
        Define.Field("currency", CurrencyType, "The currency for the amount.", fun _ h -> currencyCode h)
    ])

let BidType =
  Define.Object<Bid>(
    name = "Bid",
    description = "A bid",
    isTypeOf = (fun o -> o :? Bid),
    fieldsFn = fun () ->
    [
        Define.Field("id", Guid, "Bid id.", fun _ h -> BidId.unwrap h.id)
        Define.Field("amount", AmountType, "The bid amount.", fun _ h -> h.amount)
        Define.Field("auctionId", String, "The auction id.", fun _ h -> string h.auction)
        Define.Field("user", String, "User that placed the bid.", fun _ h -> string h.user)
        Define.Field("at", Date, "When the bid was placed.", fun _ h -> h.at)
    ])
let rec AuctionType=
  let currencyCode (a:Auction) = Currency.code (a.currency)
  Define.Object<Auction>(
    name = "Auction",
    description = "An auction",
    isTypeOf = (fun o -> o :? Auction),
    fieldsFn = fun () ->
    [
        Define.Field("id", String, "The id of the task.", fun _ (h:Auction) -> string h.id)
        Define.Field("startsAt", Date, "When the auction starts.", fun _ h -> h.startsAt)
        Define.Field("title", String, "Title.", fun _ h -> h.title)
        Define.Field("expiry", Date, "Title.", fun _ h -> h.expiry)
        Define.Field("user", String, "User that placed owns the auction.", fun _ (h:Auction) -> string h.user)
        Define.Field("type", String, "Type of auction.", fun _ (h:Auction) -> string h.typ)
        Define.Field("currency", CurrencyType, "Type of auction.", fun _ (h:Auction) -> currencyCode h)
    ])
and StateType=
  let currencyCode (a:Auction) = Currency.code (a.currency)
  Define.Object<IState>(
    name = "State",
    description = "State of an auction",
    isTypeOf = (fun o -> o :? IState),
    fieldsFn = fun () ->
    [
        Define.Field("bids", ListOf BidType, "List of bids.", fun _ (h:IState) -> h.GetBids())
        Define.Field("hasEnded", Boolean, "has ended.", fun _ (h:IState) -> h.HasEnded())
        Define.Field("winner",Nullable String, "winner.", fun _ (h:IState) -> h.TryGetAmountAndWinner() |> Option.map (snd >>string) )
        Define.Field("winningAmount", Nullable AmountType, "", fun _ (h:IState) -> h.TryGetAmountAndWinner() |> Option.map (fst))
    ])