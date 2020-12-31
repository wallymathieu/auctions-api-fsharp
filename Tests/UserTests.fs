namespace Tests
open Auctions.Domain
open System
open Xunit
open FsCheck

type ``Can parse user``() =

  [<Fact>]
  member test.``Buyer or seller``() =
    let user = BuyerOrSeller(Guid.NewGuid().ToString("N")|>UserId, "seller")
    Assert.Equal(Some user, string user |> User.TryParse )

  [<Fact>]
  member test.Support() =
    let user = Guid.NewGuid().ToString("N") |> UserId |> Support
    Assert.Equal(Some user, string user |> User.TryParse)

  [<Fact>]
  member test.``Support roundtrip``() =
    let roundtrip (u) =
      let orig = UserId u |> Support
      let parsed = orig |> string |> User.TryParse
      Some orig ?=? parsed
    fsCheck "roundtrip" (Prop.forAll Arb.nonNullAndNotVerticalBarOrNewLineOrSpace roundtrip)
  [<Fact>]
  member test.``Buyer or seller roundtrip``() =
    let roundtrip (NonNullAndNotVerticalBarOrNewLineOrSpace u) (NonNullAndNotNewlineOrSpace name)=
      let orig = BuyerOrSeller (UserId u, name)
      let parsed = orig |> string |> User.TryParse
      Some orig ?=? parsed
    fsCheck "roundtrip" roundtrip
