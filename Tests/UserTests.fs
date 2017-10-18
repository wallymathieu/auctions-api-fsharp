namespace Tests
open Auctions.Domain
open System
open Xunit

type ``Can parse user``() = 
  
  [<Fact>]
  member test.``Buyer or seller``() = 
    let user = BuyerOrSeller(Guid.NewGuid().ToString("N"), "seller")
    Assert.Equal(Some user, user.ToString() |> User.tryParse )
  
  [<Fact>]
  member test.Support() = 
    let user = Support(Guid.NewGuid().ToString("N"))
    Assert.Equal(Some user, user.ToString() |> User.tryParse)
