module Tests.ApiSpec
open System.Net
open Auctions.Web
open Auctions.Actors
open FSharpPlus.Data
open Suave
open Suave.Testing
open Xunit
open FsUnit.Xunit
open Tests.TestsModule.Json

let auctionAndStates = []
let onIncomingCommand = ignore
let observeCommandResult = ignore
let time ()= System.DateTime(2022,8,4)

// start suave
let app () = OptionT.run << webPart ( AuctionDelegator.create(auctionAndStates, onIncomingCommand, time, observeCommandResult) )
let withAuthHeader (auth:string) (req:Http.HttpRequestMessage) =
  req.Headers.Add ("x-jwt-payload",auth)
  req
let reqWithAuth method resource data auth =
  reqResp method resource "" data None DecompressionMethods.None (withAuthHeader auth) contentString

let runWebPart app = runWith defaultConfig app
let firstAuctionRequest ="""{
    "id": 1,
    "startsAt": "2022-07-01T10:00:00.000Z",
    "endsAt": "2022-09-18T10:00:00.000Z",
    "title": "Some auction",
    "currency": "VAC"
}"""
let seller1 = "eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo="
let buyer1 = "eyJzdWIiOiJhMiIsICJuYW1lIjoiQnV5ZXIiLCAidV90eXAiOiIwIn0K"
[<Fact>]
let ``create auction 1``() =
  use data = new System.Net.Http.StringContent(firstAuctionRequest)
  //
  let res =
        (runWebPart (app()))
        |> reqWithAuth HttpMethod.POST "/auction" (Some data) seller1

  assertStrJsonEqual("""{
      "$type": "AuctionAdded",
      "at": "2022-08-04T15:42:09.540Z",
      "auction": {
          "id": 1,
          "startsAt": "2022-07-01T10:00:00.000Z",
          "title": "Some auction",
          "expiry": "2022-09-18T10:00:00.000Z",
          "user": "BuyerOrSeller|a1|Test",
          "type": "English|VAC0|VAC0|0",
          "currency": "VAC"
      }
  }""", res)

let secondAuctionRequest ="""{
    "id": 2,
    "startsAt": "2021-12-01T10:00:00.000Z",
    "endsAt": "2022-12-18T10:00:00.000Z",
    "title": "Some auction",
    "currency": "VAC"
}"""

[<Fact>]
let ``create auction 2``() =
  use data = new System.Net.Http.StringContent(secondAuctionRequest)
  let res =
        (runWebPart (app()))
        |> reqWithAuth HttpMethod.POST "/auction" (Some data) seller1

  assertStrJsonEqual("""{
      "$type": "AuctionAdded",
      "at": "2022-08-04T15:42:25.282Z",
      "auction": {
          "id": 2,
          "startsAt": "2021-12-01T10:00:00.000Z",
          "title": "Some auction",
          "expiry": "2022-12-18T10:00:00.000Z",
          "user": "BuyerOrSeller|a1|Seller",
          "type": "English|VAC0|VAC0|0",
          "currency": "VAC"
      }
  }""", res)

[<Fact>]
let ``Place bid as buyer on auction 1``() =
  let app = app()
  use auctionReq = new System.Net.Http.StringContent(firstAuctionRequest)
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction" (Some auctionReq) seller1 |> ignore
  use bidReq = new System.Net.Http.StringContent("""{ "auction":"1","amount":"VAC11" }""")
  let res =
        (runWebPart app)
        |> reqWithAuth HttpMethod.POST "/auction/1/bid" (Some bidReq) buyer1

  assertStrJsonEqual("""{
    "$type": "BidAccepted",
    "at": "2022-08-04T15:43:54.895Z",
    "bid": {
        "id": "a3b0e7f394f7457085d3788b8fb7f8ee",
        "auction": 1,
        "user": "BuyerOrSeller|a2|Buyer",
        "amount": "VAC11",
        "at": "2022-08-04T15:43:54.837Z"
    }
}""", res)

[<Fact>]
let ``Place bid as buyer on auction 2``() =
  let app = app()
  use auctionReq = new System.Net.Http.StringContent(secondAuctionRequest)
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction" (Some auctionReq) seller1 |> ignore
  use bidReq = new System.Net.Http.StringContent("""{ "amount":"VAC11" }""")
  let res =
        (runWebPart app)
        |> reqWithAuth HttpMethod.POST "/auction/2/bid" (Some bidReq) buyer1

  assertStrJsonEqual("""{
    "$type": "BidAccepted",
    "at": "2022-08-04T15:44:05.554Z",
    "bid": {
        "id": "c523b4d3a9e44c25ba4f87b2927999b0",
        "auction": 2,
        "user": "BuyerOrSeller|a2|Buyer",
        "amount": "VAC11",
        "at": "2022-08-04T15:44:05.530Z"
    }
}""", res)

[<Fact>]
let ``Place bid as seller on auction 1``() =
  let app = app()
  use auctionReq = new System.Net.Http.StringContent(firstAuctionRequest)
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction" (Some auctionReq) seller1 |> ignore
  use bidReq = new System.Net.Http.StringContent("""{ "amount":"VAC11" }""")
  let res =
        (runWebPart app)
        |> reqWithAuth HttpMethod.POST "/auction/1/bid" (Some bidReq) seller1

  assertStrJsonEqual("""{
    "type": "SellerCannotPlaceBids",
    "userId": "a1",
    "auctionId": 1
}""", res)

[<Fact>]
let ``get auctions``() =
(*
127.0.0.1:8083/auction/1
x-jwt-payload: eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo=
{
    "id": 1,
    "startsAt": "2022-07-01T10:00:00.000Z",
    "title": "Some auction",
    "expiry": "2022-09-18T10:00:00.000Z",
    "currency": "VAC",
    "bids": [
        {
            "amount": "VAC11",
            "bidder": "BuyerOrSeller|a2|Buyer"
        }
    ],
    "winner": "",
    "winnerPrice": ""
}
*)
  //Assert.Equal(auctionId |> AuctionHasEnded |> Error, timedAscState |> S.addBid bid |> snd )
  ()