module Tests.WebTests
open Auctions
open Auctions.Domain
open System
open Xunit
open Fleece
open Fleece.FSharpData
open Auctions.Web
let inline assertOkWithValue (expectedValue:'a, actualValue:Result<'a,_>) =
  match actualValue with
  | Ok v -> Assert.Equal<'a>(expectedValue, v)
  | Error e -> failwithf $"Did not expect error {e}"

module ``JwtPayload deserialization spec``=

  let ``seller``: JwtPayload ParseResult = ofJsonText """{"sub":"a1", "name":"Seller", "u_typ":"0"}"""
  [<Fact>]
  let ``can parse and interpret seller``() =
      assertOkWithValue ({ user = BuyerOrSeller (UserId "a1", "Seller") }, seller)
  let decodeXJwtPayloadHeader (value:string)=
      value
      |> Convert.FromBase64String
      |> System.Text.Encoding.UTF8.GetString
      |> ofJsonText
  let ``base64 encoded seller`` = decodeXJwtPayloadHeader "eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo="
  [<Fact>]
  let ``can parse base64 and interpret seller``() =
      assertOkWithValue ({ user = BuyerOrSeller (UserId "a1", "Test") }, ``base64 encoded seller``)
  let ``base64 encoded buyer`` = decodeXJwtPayloadHeader "eyJzdWIiOiJhMiIsICJuYW1lIjoiQnV5ZXIiLCAidV90eXAiOiIwIn0K"
  [<Fact>]
  let ``can parse base64 and interpret buyer``() =
      assertOkWithValue ({ user = BuyerOrSeller (UserId "a2", "Buyer") }, ``base64 encoded buyer``)

  let ``seller without a name``: JwtPayload ParseResult = ofJsonText """{"sub":"a1", "u_typ":"0"}"""

  [<Fact>]
  let ``cannot parse seller without a name``() =
    match ``seller without a name`` with
    | Ok user -> failwithf $"Did not expect user %A{user}"
    | Error (InvalidValue _) -> ()
    | Error err -> failwithf $"Did not expect error %A{err}"

  let ``support with name`` : JwtPayload ParseResult = ofJsonText """{"sub":"a1", "name":"Support", "u_typ":"1"}"""

  [<Fact>]
  let ``can parse support with name``() =
    assertOkWithValue ({ user = Support (UserId "a1") }, ``support with name``)

  let ``support with name with newlines`` : JwtPayload ParseResult = ofJsonText """{
  "sub":"a1",
  "name":"Test",
  "u_typ":"1"
}
"""
  [<Fact>]
  let ``can parse support with name and with newlines``() =
    match ``support with name with newlines`` with
    | Ok v -> Assert.Equal({ user = Support (UserId "a1") }, v)
    | Error e -> failwithf $"Did not expect error {e}"

  let ``support without name`` : JwtPayload ParseResult = ofJsonText """{"sub":"a1", "u_typ":"1"}"""

  [<Fact>]
  let ``can parse and interpret support without name``() =
    assertOkWithValue ({ user = Support (UserId "a1") }, ``support without name``)

  let ``from json without type`` : JwtPayload ParseResult = ofJsonText """{"sub":"a1", "name":"Support"}"""

  [<Fact>]
  let ``missing type``() =
    match ``from json without type`` with
    | Ok user -> failwithf $"Did not expect user %A{user}"
    | Error (PropertyNotFound _) -> ()
    | Error err -> failwithf $"Did not expect error %A{err}"

  let ``from json without invalid type`` : JwtPayload ParseResult = ofJsonText """{"sub":"a1", "name":"Support", "u_typ":"100"}"""

  [<Fact>]
  let ``invalid type``() =
    match ``from json without invalid type`` with
    | Ok user -> failwithf $"Did not expect user %A{user}"
    | Error (InvalidValue _) -> ()
    | Error err -> failwithf $"Did not expect error %A{err}"

open TestData
module ``bid deserialization spec``=
  let ``bid of 10`` = OfJson.bidReq (auctionId,buyer,endsAt) (JsonValue.Parse """{ "amount":10 }""")

  [<Fact>]
  let ``can parse bid request``() =
    assertOkWithValue ({ user = buyer; amount=10
                         auction=auctionId; at = endsAt }, ``bid of 10``)

module ``auction deserialization spec``=
  let ``add first auction`` = OfJson.addAuctionReq seller (JsonValue.Parse """{ "id":1,"startsAt":"2016-01-01T00:00:00.000Z","endsAt":"2016-02-01T00:00:00.000Z","title":"First auction", "currency":"VAC" }""")

  [<Fact>]
  let ``can add auction request``() =
    match ``add first auction`` with
    | Ok auctionReq ->
        let expected = { id = auctionId
                         title = "First auction"
                         startsAt = startsAt; expiry = endsAt
                         currency = Currency.VAC
                         user = seller
                         typ = TimedAscending { reservePrice = AmountValue.zero
                                                minRaise = AmountValue.zero
                                                timeFrame = TimeSpan.Zero }
                         openBidders = false }
        Assert.Equal(expected, auctionReq)
    | Error err-> failwithf $"Did not expect error %A{err}"
