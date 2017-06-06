module Either
open FSharp.Core

type EitherBuilder() = 
  member this.Bind(x, f) = Result.bind f x
  member this.ReturnFrom x = x
  member this.Zero() = Result.Ok()

let either = new EitherBuilder()
