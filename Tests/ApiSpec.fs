module Tests.ApiSpec
open System
open System.Net
open System.Text
open Auctions.Web
open Auctions.Actors
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.Logging
open Xunit
open Tests.TestsModule.Json
open System.Net.Http
open Giraffe

let auctionAndStates = []
let onIncomingCommand = ignore
let observeCommandResult = ignore
let time ()= DateTime(2022,8,4)
let appJson = "application/json"

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// start Giraffe
let app () =
  let agent = AuctionDelegator.create(auctionAndStates, onIncomingCommand, time, observeCommandResult)
  let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
       .UseGiraffe (webPart agent time)

  new TestServer(WebHostBuilder()
       .UseKestrel()
       .UseContentRoot(IO.Directory.GetCurrentDirectory())
       .Configure configureApp)


let withAuthHeader (auth:string) (req:HttpRequestMessage) =
  req.Headers.Add ("x-jwt-payload", auth)

let reqWithAuth (method:HttpMethod) resource data (auth: string) (server: TestServer) =
  let runTask (t:System.Threading.Tasks.Task<'t>) = t.GetAwaiter().GetResult()
  let res =
    server
      .CreateRequest(resource)
      .And(fun r->
        withAuthHeader auth r
        match data with
        | Some d-> r.Content <- d
        | None -> ()
      )
      .SendAsync(method.Method)
      |> runTask
  let content = res.Content.ReadAsStringAsync() |> runTask
  (res.StatusCode, content)

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
  use data = new StringContent(firstAuctionRequest, Encoding.UTF8, appJson)
  //
  let statusCode,res =
        app() |> reqWithAuth HttpMethod.Post "/auctions" (Some data) seller1
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
          "type": "English|0|0|0",
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
  use data = new StringContent(secondAuctionRequest, Encoding.UTF8, appJson)
  let statusCode,res =
        app() |> reqWithAuth HttpMethod.Post "/auctions" (Some data) seller1
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
          "type": "English|0|0|0",
          "currency": "VAC",
          "open": false
      }
  }""", res)

[<Fact>]
let ``Place bid as buyer on auction 1``() =
  let app = app()
  use auctionReq = new StringContent(firstAuctionRequest, Encoding.UTF8, appJson)
  app |> reqWithAuth HttpMethod.Post "/auctions" (Some auctionReq) seller1 |> ignore

  use bidReq = new StringContent("""{ "amount":11 }""", Encoding.UTF8, appJson)
  let statusCode,res =
        app |> reqWithAuth HttpMethod.Post "/auctions/1/bids" (Some bidReq) buyer1

  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
    "$type": "BidAccepted",
    "at": "2022-08-04T00:00:00.000Z",
    "bid": {
        "auction": 1,
        "user": "BuyerOrSeller|a2|Buyer",
        "amount": 11,
        "at": "2022-08-04T00:00:00.000Z"
    }
}""", res)

[<Fact>]
let ``Place bid as buyer on auction 2``() =
  let app = app()
  use auctionReq = new StringContent(secondAuctionRequest, Encoding.UTF8, appJson)
  app |> reqWithAuth HttpMethod.Post "/auctions" (Some auctionReq) seller1 |> ignore
  use bidReq = new StringContent("""{ "amount":11 }""")
  let statusCode,res =
        app |> reqWithAuth HttpMethod.Post "/auctions/2/bids" (Some bidReq) buyer1
  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
    "$type": "BidAccepted",
    "at": "2022-08-04T00:00:00.000Z",
    "bid": {
        "auction": 2,
        "user": "BuyerOrSeller|a2|Buyer",
        "amount": 11,
        "at": "2022-08-04T00:00:00.000Z"
    }
}""", res)

[<Fact>]
let ``Place bid as seller on auction 1``() =
  let app = app()
  use auctionReq = new StringContent(firstAuctionRequest, Encoding.UTF8, appJson)
  app |> reqWithAuth HttpMethod.Post "/auctions" (Some auctionReq) seller1 |> ignore
  use bidReq = new StringContent("""{ "amount":11 }""")
  let statusCode,res =
        app |> reqWithAuth HttpMethod.Post "/auctions/1/bids" (Some bidReq) seller1
  Assert.Equal (HttpStatusCode.BadRequest, statusCode)
  assertStrJsonEqual ("""{
    "type": "SellerCannotPlaceBids",
    "userId": "a1",
    "auctionId": 1
}""", res)

[<Fact>]
let ``get auctions``() =
  let app = app()
  use auctionReq = new StringContent(firstAuctionRequest, Encoding.UTF8, appJson)
  app |> reqWithAuth HttpMethod.Post "/auctions" (Some auctionReq) seller1 |> ignore
  use bidReq = new StringContent("""{ "amount":11 }""", Encoding.UTF8, appJson)
  app |> reqWithAuth HttpMethod.Post "/auctions/1/bids" (Some bidReq) buyer1 |> ignore

  let statusCode,res =
        app |> reqWithAuth HttpMethod.Get "/auctions/1" None seller1
  Assert.Equal (HttpStatusCode.OK, statusCode)
  assertStrJsonEqual ("""{
    "id": 1,
    "startsAt": "2022-07-01T10:00:00.000Z",
    "title": "Some auction",
    "expiry": "2022-09-18T10:00:00.000Z",
    "currency": "VAC",
    "bids": [
        {
            "amount": 11,
            "bidder": "BuyerOrSeller|a2|Buyer"
        }
    ],
    "winner": "",
    "winnerPrice": ""
}""", res)
