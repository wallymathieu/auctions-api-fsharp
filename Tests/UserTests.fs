namespace Tests
open Auctions.Domain
open System
open Xunit

type ``Can parse user``() = 
  
  [<Fact>]
  member test.``Buyer or seller``() = 
    let user = BuyerOrSeller(Guid.NewGuid(), "seller")
    Assert.Equal(user, user.ToString() |> User.parse)
  
  [<Fact>]
  member test.Support() = 
    let user = Support(Guid.NewGuid())
    Assert.Equal(user, user.ToString() |> User.parse)
