﻿[<AutoOpen>]
module Tests.TestsModule
open Auctions.Domain
open System
open FSharpPlus
open FsCheck
open System.Text.RegularExpressions

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
   }

let auction : Auction = auctionOfTyp (SingleSealedBid Vickrey)

let sek a =
  { value = a
    currency = Currency CurrencyCode.SEK }

let bid =
    { id = BidId.New()
      auction = auctionId
      user = buyer
      amount = sek 100L
      at = DateTime(2016, 1, 2) }

let buyer1 = BuyerOrSeller(UserId "x2", "Buyer")
let buyer2 = BuyerOrSeller(UserId "x3", "Buyer")

let bid1 = { id = BidId.New()
             auction =auctionId
             user=buyer1
             amount =parse "SEK10"
             at = startsAt.AddHours(1.0)
           }
let bid2 = { id = BidId.New()
             auction =auctionId
             user=buyer2
             amount =parse "SEK12"
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

module Arb =
  let nonNullAndNotNewlineOrSpace =
    Arb.from<string> |> Arb.filter String.isNotNewlineOrSpace
  let nonNullAndNotVerticalBarOrNewLineOrSpace =
    Arb.from<string> |> Arb.filter String.isNotVerticalBarNewlineOrSpace
  /// Generate amounts with known currency codes and positive integers as values
  let amount =
    let positiveInt = Arb.toGen Arb.from<PositiveInt>
    let toAmount (c, PositiveInt i) = { value=int64 i;currency=c }
    let ofAmount { value=i;currency=c } = (c, PositiveInt (int i))
    Arb.fromGenShrink (Gen.zip Gen.currency positiveInt, Arb.shrink) |> Arb.convert toAmount ofAmount

  /// Arbitrary buyer or sellers with restricted values for userId and usernames
  let buyerOrSeller =
    let filter (userId,username) = String.isNotVerticalBarNewlineOrSpace userId && String.isNotNewlineOrSpace username
    let toBuyerOrSeller (userId,username) = BuyerOrSeller (UserId userId,username)
    let ofBuyerOrSeller = function | BuyerOrSeller (UserId userId,username) -> userId,username
    Arb.from<string*string>
    |> Arb.filter filter
    |> Arb.convert toBuyerOrSeller ofBuyerOrSeller
  /// Arbitrary support users with restricted values for userId
  let support =
    let toSupport userId = Support (UserId userId)
    let ofSupport = function | Support (UserId userId) -> userId
    Arb.from<string>
    |> Arb.filter String.isNotVerticalBarNewlineOrSpace
    |> Arb.convert toSupport ofSupport

let inline (?=?) left right = left = right |@ sprintf "%A = %A" left right

let fsCheck x = Check.One({Config.QuickThrowOnFailure with Name = "";  }, x)
