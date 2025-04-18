﻿namespace Tests
open FSharpPlus
open Xunit
/// These tests are done to verify that using monad CE with result works as expected
module ``Result tests`` =
  let errorIfFalse v = if v then Ok () else Error ()
  let func param sideEffect= monad {
                         do! errorIfFalse param
                         sideEffect()
                         return param
                       }
  let returnsOk ()=Ok ()
  let func2 param sideEffect= monad {
                         do! returnsOk()
                         do! errorIfFalse param
                         sideEffect()
                         return param
                       }
  let func3 param sideEffect= monad {
                         do! errorIfFalse param
                         do! returnsOk()
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

  [<Fact>]
  let ``Should return correct value of failing path even if there are two _1``() =
    let mutable count =0
    let sideEffect ()=count<-count+1
    let res = func2 false sideEffect
    Assert.Equal(Error (), res)
    Assert.Equal(0, count)

  [<Fact>]
  let ``Should return correct value of failing path even if there are two _2``() =
    let mutable count =0
    let sideEffect ()=count<-count+1
    let res = func3 false sideEffect
    Assert.Equal(Error (), res)
    Assert.Equal(0, count)
