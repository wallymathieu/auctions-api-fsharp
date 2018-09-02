namespace Tests

open Auctions.Domain

open System
open Xunit

module ``Amount tests`` = 
  let amountMax = {value=12L; currency=Currency.VAC}
  let amounts = [ 
                  {value=1L; currency=Currency.VAC}
                  amountMax
                  {value=6L; currency=Currency.VAC}
                  {value=10L; currency=Currency.VAC}
                ]
  [<Fact>]
  let ``Can find the max amount``() = 
    let max = amounts |> List.max
    Assert.Equal (amountMax, max)

  [<Fact>]
  let ``Can parse currency``() = 
    Assert.Equal (Some (Currency CurrencyCode.SEK), Currency.tryParse("SEK"))
    
  [<Fact>]
  let ``Currency to string``() = 
    Assert.Equal ("SEK", string (Currency CurrencyCode.SEK))

