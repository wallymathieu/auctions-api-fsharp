module Auctions.Either

type EitherBuilder() = 
  member this.Bind(x, f) = Result.bind f x
  member this.ReturnFrom x = x
  member this.Zero() = Result.Ok()
  member this.Return(x) = Result.Ok(x)
  member this.Combine (a: Result<'a,_>,b:Result<'b,_>) = 
    match a,b with
    | Error err, _ -> Error err
    | _ , Error err -> Error err
    | Ok a1, Ok b2 -> Ok (a1,b2)

  member this.Combine (a: Result<unit,_>,b:Result<'a,_>) = 
    match a,b with
    | Error err, _ -> Error err
    | _ , Error err -> Error err
    | Ok (), Ok b1 -> Ok b1
  member this.Combine (a: Result<'a,_>,b:Result<unit,_>) = 
    match a,b with
    | Error err, _ -> Error err
    | _ , Error err -> Error err
    | Ok a1, Ok () -> Ok a1
  member this.Yield x= 
    Ok x
  member this.YieldFrom x= 
    x

  member this.Delay f= f()

let either = new EitherBuilder()
