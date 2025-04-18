module Tests.ApiSpec
open System.Net
open Auctions.Web
open Auctions.Actors
open FSharpPlus.Data
open Suave
open Suave.Testing
open Xunit
open Tests.TestsModule.Json
open System.Net.Http

let auctionAndStates = []
let onIncomingCommand = ignore
let observeCommandResult = ignore
let time ()= System.DateTime(2022,8,4)

// start suave
let app () = OptionT.run << webPart ( AuctionDelegator.create(auctionAndStates, onIncomingCommand, time, observeCommandResult) ) time
let withAuthHeader (auth:string) (req:HttpRequestMessage) =
  req.Headers.Add ("x-jwt-payload",auth)
  req
let reqWithAuth method resource data auth =
  let contentStringAndStatusCode (m: HttpResponseMessage) = (statusCode m,contentString m)
  reqResp method resource "" data None DecompressionMethods.None (withAuthHeader auth) contentStringAndStatusCode

let runWebPart app = runWith defaultConfig app
let firstAuctionRequest ="""{
    "id": 1,
    "startsAt": "2022-07-01T10:00:00.000Z",
    "endsAt": "2022-09-18T10:00:00.000Z",
    "title": "Some auction",
    "currency": "VAC",
    "open": true
}"""
let seller1 = "eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo="
let buyer1 = "eyJzdWIiOiJhMiIsICJuYW1lIjoiQnV5ZXIiLCAidV90eXAiOiIwIn0K"
[<Fact>]
let ``create auction 1``() =
  use data = new StringContent(firstAuctionRequest)
  //
  let statusCode,res =
        (runWebPart (app()))
        |> reqWithAuth HttpMethod.POST "/auction" (Some data) seller1
  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
      "$type": "AuctionAdded",
      "at": "2022-08-04T00:00:00.000Z",
      "auction": {
          "id": 1,
          "startsAt": "2022-07-01T10:00:00.000Z",
          "title": "Some auction",
          "expiry": "2022-09-18T10:00:00.000Z",
          "user": "BuyerOrSeller|a1|Test",
          "type": "English|VAC0|VAC0|0",
          "currency": "VAC",
          "open": true
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
  use data = new StringContent(secondAuctionRequest)
  let statusCode,res =
        (runWebPart (app()))
        |> reqWithAuth HttpMethod.POST "/auction" (Some data) seller1
  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
      "$type": "AuctionAdded",
      "at": "2022-08-04T00:00:00.000Z",
      "auction": {
          "id": 2,
          "startsAt": "2021-12-01T10:00:00.000Z",
          "title": "Some auction",
          "expiry": "2022-12-18T10:00:00.000Z",
          "user": "BuyerOrSeller|a1|Test",
          "type": "English|VAC0|VAC0|0",
          "currency": "VAC",
          "open": false
      }
  }""", res)

[<Fact>]
let ``Place bid as buyer on auction 1``() =
  let app = app()
  use auctionReq = new StringContent(firstAuctionRequest)
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction" (Some auctionReq) seller1 |> ignore

  use bidReq = new StringContent("""{ "auction":"1","amount":"VAC11" }""")
  let statusCode,res =
        (runWebPart app)
        |> reqWithAuth HttpMethod.POST "/auction/1/bid" (Some bidReq) buyer1

  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
    "$type": "BidAccepted",
    "at": "2022-08-04T00:00:00.000Z",
    "bid": {
        "auction": 1,
        "user": "BuyerOrSeller|a2|Buyer",
        "amount": "VAC11",
        "at": "2022-08-04T00:00:00.000Z"
    }
}""", res)

[<Fact>]
let ``Place bid as buyer on auction 2``() =
  let app = app()
  use auctionReq = new StringContent(secondAuctionRequest)
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction" (Some auctionReq) seller1 |> ignore
  use bidReq = new StringContent("""{ "amount":"VAC11" }""")
  let statusCode,res =
        (runWebPart app)
        |> reqWithAuth HttpMethod.POST "/auction/2/bid" (Some bidReq) buyer1
  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
    "$type": "BidAccepted",
    "at": "2022-08-04T00:00:00.000Z",
    "bid": {
        "auction": 2,
        "user": "BuyerOrSeller|a2|Buyer",
        "amount": "VAC11",
        "at": "2022-08-04T00:00:00.000Z"
    }
}""", res)

[<Fact>]
let ``Place bid as seller on auction 1``() =
  let app = app()
  use auctionReq = new StringContent(firstAuctionRequest)
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction" (Some auctionReq) seller1 |> ignore
  use bidReq = new StringContent("""{ "amount":"VAC11" }""")
  let statusCode,res =
        (runWebPart app)
        |> reqWithAuth HttpMethod.POST "/auction/1/bid" (Some bidReq) seller1
  Assert.Equal (HttpStatusCode.BadRequest, statusCode)
  assertStrJsonEqual ("""{
    "type": "SellerCannotPlaceBids",
    "userId": "a1",
    "auctionId": 1
}""", res)

[<Fact>]
let ``get auctions``() =
  let app = app()
  use auctionReq = new StringContent(firstAuctionRequest)
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction" (Some auctionReq) seller1 |> ignore
  use bidReq = new StringContent("""{ "auction":"1","amount":"VAC11" }""")
  (runWebPart app)
  |> reqWithAuth HttpMethod.POST "/auction/1/bid" (Some bidReq) buyer1 |> ignore

  let statusCode,res =
        (runWebPart app)
        |> reqWithAuth HttpMethod.GET "/auction/1" None seller1
  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
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
}""", res)
