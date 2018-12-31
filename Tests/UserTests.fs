namespace Tests
open Auctions.Domain
open System
open Xunit

type ``Can parse user``() = 
  
  [<Fact>]
  member test.``Buyer or seller``() = 
    let user = BuyerOrSeller(Guid.NewGuid().ToString("N")|>UserId, "seller")
    Assert.Equal(Some user, string user |> User.tryParse )
  
  [<Fact>]
  member test.Support() = 
    let user = Guid.NewGuid().ToString("N") |> UserId |> Support
    Assert.Equal(Some user, string user |> User.tryParse)
