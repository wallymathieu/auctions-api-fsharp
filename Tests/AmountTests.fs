namespace Tests

open Auctions.Domain
open Auctions.Commands
open Auctions.Either
open Auctions
open System
open Xunit

module ``Amount tests`` = 
  let amountMax = {value=12.0; currency=Currency.VAC}
  let amounts = [ 
                  {value=1.0; currency=Currency.VAC}
                  amountMax
                  {value=6.0; currency=Currency.VAC}
                  {value=10.0; currency=Currency.VAC}
                ]
  [<Fact>]
  let ``Can find the max amount``() = 
    let max = amounts |> List.max
    Assert.Equal (amountMax, max)

  [<Fact>]
  let ``Can parse currency``() = 
    Assert.Equal (Currency.SEK, Currency.Parse("SEK"))
    
