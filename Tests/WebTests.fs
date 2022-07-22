module Tests.WebTests
open Auctions
open Auctions.Domain
open System
open Xunit
open Fleece
open Fleece.FSharpData
open Auctions.Web
module ``JwtPayload deserialization spec``=
  let ``seller``: JwtPayload ParseResult = ofJsonText """{"sub":"a1", "name":"Seller", "u_typ":"0"}"""

  [<Fact>]
  let ``can parse and interpret seller``() =
      Assert.Equal(Ok <| { user = BuyerOrSeller (UserId "a1", "Seller") }, seller)

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
    Assert.Equal(Ok <| { user = Support (UserId "a1") }, ``support with name``)

  let ``support without name`` : JwtPayload ParseResult = ofJsonText """{"sub":"a1", "u_typ":"1"}"""

  [<Fact>]
  let ``can parse and interpret support without name``() =
    Assert.Equal(Ok <| { user = Support (UserId "a1") }, ``support without name``)

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
  let ``bid of 10 VAC`` = OfJson.bidReq (auctionId,buyer,endsAt) (JsonValue.Parse """{ "amount":"VAC10" }""")

  [<Fact>]
  let ``can parse bid request``() =
    match ``bid of 10 VAC`` with
    | Ok bidReq ->
        let emptyBidId = BidId.BidId Guid.Empty
        Assert.Equal({ user = buyer; id = emptyBidId; amount={ value=10; currency=Currency.VAC }
                       auction=auctionId; at = endsAt }, { bidReq with id = emptyBidId} )
    | Error err-> failwithf $"Did not expect error %A{err}"

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
                         typ = TimedAscending { reservePrice = Amount.zero Currency.VAC
                                                minRaise = Amount.zero Currency.VAC
                                                timeFrame = TimeSpan.Zero } }
        Assert.Equal(expected, auctionReq)
    | Error err-> failwithf $"Did not expect error %A{err}"
