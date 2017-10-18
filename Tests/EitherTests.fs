namespace Tests

open Auctions.Domain
open Auctions.Commands
open Auctions.Either
open Auctions
open System
open Xunit

module ``Either tests`` = 
  let errorIfFalse v = if v then Ok () else Error ()
  let func param sideEffect= either { 
                         do! errorIfFalse param
                         sideEffect()
                         return param
                       }
  [<Fact>]
  let ``Should return correct value of happy path``() = 
    let mutable count =0
    let sideEffect ()=count<-count+1
    let res = func true sideEffect
    Assert.Equal(Ok true, res)
    Assert.Equal(1, count)

  [<Fact>]
  let ``Should return correct value of failing path``() = 
    let mutable count =0
    let sideEffect ()=count<-count+1
    let res = func false sideEffect
    Assert.Equal(Error (), res)
    Assert.Equal(0, count)
