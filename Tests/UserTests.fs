namespace Tests
open Auctions.Domain
open System
open Xunit
open FsCheck
open FsCheck.FSharp

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
    let roundtrip orig =
      let parsed = orig |> string |> User.TryParse
      Some orig ?=? parsed
    fsCheck (Prop.forAll Arbitrary.support roundtrip)
  [<Fact>]
  member test.``Buyer or seller roundtrip``() =
    let roundtrip orig =
      let parsed = orig |> string |> User.TryParse
      Some orig ?=? parsed
    fsCheck (Prop.forAll Arbitrary.buyerOrSeller roundtrip)
