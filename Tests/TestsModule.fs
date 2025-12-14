[<AutoOpen>]
module Tests.TestsModule
open Auctions.Domain
open System
open FSharpPlus
open FsCheck
open FsCheck.FSharp
open System.Text.RegularExpressions
module TestData =
  let auctionId = AuctionId 1L
  let title = "auction"
  let startsAt = DateTime(2016, 1, 1)
  let endsAt = DateTime(2016, 2, 1)
  let seller = BuyerOrSeller(UserId "x1", "Seller")
  let buyer = BuyerOrSeller(UserId "x2", "Buyer")
  let auctionOfTyp typ : Auction =
    { id = auctionId
      title = title
      startsAt = startsAt
      expiry = endsAt
      user = seller
      currency=Currency CurrencyCode.SEK
      typ= typ
      openBidders = false
     }

  let auction : Auction = auctionOfTyp (SingleSealedBid Vickrey)

  let sek a =
    { value = a
      currency = Currency CurrencyCode.SEK }

  let bid =
      { auction = auctionId
        user = buyer
        amount = 100L
        at = DateTime(2016, 1, 2) }

  let buyer1 = BuyerOrSeller(UserId "x2", "Buyer")
  let buyer2 = BuyerOrSeller(UserId "x3", "Buyer")

  let bid1 = { auction =auctionId
               user=buyer1
               amount =parse "10"
               at = startsAt.AddHours(1.0)
             }
  let bid2 = { auction =auctionId
               user=buyer2
               amount =parse "12"
               at = startsAt.AddHours(2.0)
             }

  let addBidsToState state=
    state
    |> S.addBid bid1 |> fst
    |> S.addBid bid2 |> fst

// useful for debugging
let tee on something=
  on something
  something

module String =
  let isNotNullOrEmpty s = not <| String.IsNullOrEmpty s
  let doesNotContainNewline (s:string) = not <| s.Contains "\n"
  let isNotNewlineOrSpace s = isNotNullOrEmpty s && Regex.IsMatch (s, "^[^ \f\n\r\t\v]+$")
                              && doesNotContainNewline s

  let isNotVerticalBarNewlineOrSpace s = isNotNullOrEmpty s && Regex.IsMatch (s, "^[^| \f\n\r\t\v]+$")
                                         // seems that ending with a newline is matched by the Regex engine
                                         && doesNotContainNewline s
module Gen =
    /// Generate currency with currency codes that exists in the enumeration
    let currency =
      Gen.elements (Seq.cast<CurrencyCode> (Enum.GetValues typeof<CurrencyCode>)) |> Gen.map Currency

module Arbitrary =
  let nonNullAndNotNewlineOrSpace =
    ArbMap.defaults |> ArbMap.arbitrary<string> |> Arb.filter String.isNotNewlineOrSpace
  let nonNullAndNotVerticalBarOrNewLineOrSpace =
    ArbMap.defaults |> ArbMap.arbitrary<string> |> Arb.filter String.isNotVerticalBarNewlineOrSpace
  /// Generate amounts with known currency codes and positive integers as values
  let amount =
    let positiveInt = Arb.toGen (ArbMap.defaults |> ArbMap.arbitrary<PositiveInt>)
    let toAmount (c, PositiveInt i) = { value=int64 i;currency=c }
    let ofAmount { value=i;currency=c } = (c, PositiveInt (int i))
    Arb.fromGenShrink (Gen.zip Gen.currency positiveInt, (ArbMap.defaults |> ArbMap.arbitrary<_> |> Arb.toShrink)) |> Arb.convert toAmount ofAmount

  /// Arbitrary buyer or sellers with restricted values for userId and usernames
  let buyerOrSeller =
    let filter (userId,username) = String.isNotVerticalBarNewlineOrSpace userId && String.isNotNewlineOrSpace username
    let toBuyerOrSeller (userId,username) = BuyerOrSeller (UserId userId,username)
    let ofBuyerOrSeller = function | BuyerOrSeller (UserId userId,username) -> userId,username
    ArbMap.defaults |> ArbMap.arbitrary<string*string>
    |> Arb.filter filter
    |> Arb.convert toBuyerOrSeller ofBuyerOrSeller
  /// Arbitrary support users with restricted values for userId
  let support =
    let toSupport userId = Support (UserId userId)
    let ofSupport = function | Support (UserId userId) -> userId
    ArbMap.defaults |> ArbMap.arbitrary<string>
    |> Arb.filter String.isNotVerticalBarNewlineOrSpace
    |> Arb.convert toSupport ofSupport

let inline (?=?) left right = Prop.label (sprintf "%A = %A" left right) (left = right)

let fsCheck x = Check.One(Config.QuickThrowOnFailure, x)
module Json =
  open FSharp.Data
  let rec areJsonEqual (expected: JsonValue, actual: JsonValue)=
    match expected,actual with
    | JsonValue.String e, JsonValue.String a -> e = a
    | JsonValue.Boolean e, JsonValue.Boolean a -> e = a
    | JsonValue.Float e, JsonValue.Float a -> e = a
    | JsonValue.Number e, JsonValue.Number a -> e = a
    | JsonValue.Null, JsonValue.Null -> true
    | JsonValue.Array e, JsonValue.Array a when e.Length = a.Length && e.Length = 0 -> true
    | JsonValue.Array e, JsonValue.Array a when e.Length = a.Length -> Array.zip e a |> Array.map areJsonEqual |> Array.reduce (&&)
    | JsonValue.Record e, JsonValue.Record a when e.Length = a.Length && e.Length = 0 -> true
    | JsonValue.Record e, JsonValue.Record a when e.Length = a.Length -> let orderByFirst = Array.sortBy fst
                                                                         let eq ((efst,esnd), (afst,asnd)) =
                                                                           efst = afst && areJsonEqual (esnd, asnd)
                                                                         in Array.zip (orderByFirst e) (orderByFirst a)
                                                                            |> Array.map eq |> Array.reduce (&&)
    | _ -> false
  let assertJsonEqual (expected: JsonValue, actual: JsonValue)=
    if areJsonEqual (expected, actual) then () else failwithf "The actual value %O is not equal to expected %O" actual expected
  let assertStrJsonEqual  (expected: string, actual: string)=
    assertJsonEqual (parse expected, parse actual)