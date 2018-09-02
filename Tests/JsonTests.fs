module Tests.JsonTests
open Auctions.Domain
open Xunit
open System
open Fleece
open Fleece.FSharpData
open Auctions.Web

[<Fact>]
let ``Add auction``() = 
  let a: AddAuctionReq = {
    id = 1L
    startsAt = DateTime(2018,1,1, 10,0,0)
    title = "First auction"
    endsAt = DateTime(2019,1,1, 10,0,0)
    currency = "VAC"
    typ=""
  }
  let res = parseJson @"{ ""id"":1,""startsAt"":""2018-01-01T10:00:00.000Z"",""endsAt"":""2019-01-01T10:00:00.000Z"",""title"":""First auction"", ""currency"":""VAC"" }"
  match res with
  | Ok v->Assert.Equal (a, v)
  | Error e->failwithf "%A" e

[<Fact>]
let ``Place bid``() = 
  let a: BidReq = {
    amount= Amount.parse "VAC12"
  }
  let res = parseJson @"{ ""amount"":""VAC12"" }"
  match res with
  | Ok v->Assert.Equal (a, v)
  | Error e->failwithf "%A" e
