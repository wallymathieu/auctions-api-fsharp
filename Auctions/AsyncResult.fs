[<AutoOpen>]
module Auctions.AsyncResult
open Auctions

//from https://github.com/SuaveIO/suave/blob/master/src/Suave/WebPart.fs
type AsyncResult<'a,'e> = Async<Result< 'a,'e>>

let inline succeed x = async.Return (Ok x)

let bind (f: 'a -> AsyncResult<'b,'e>) (a: AsyncResult<'a,'e>) = async {
  let! p = a
  match p with
  | Error e ->
    return Error e
  | Ok q ->
    let r = f q
    return! r
  }
let private resultAsyncBind (f: 'a -> AsyncResult<'b,'e>) (a: Result<'a,'e>) = async {
  match a with
  | Error e ->
    return Error e
  | Ok q ->
    let r = f q
    return! r
  }

type AsyncResultBuilder() =
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
  member this.Return(x:'a) : AsyncResult<'a,'e> = async { return Ok x }
  member this.Zero() : AsyncResult<unit,'e> = async { return Ok () }
  member this.ReturnFrom(x : AsyncResult<'a,'e>) = x
  member this.Delay(f: unit ->  AsyncResult<'a,'e>) = async { return! f () }
  member this.Bind(x :AsyncResult<'a,'e>, f : 'a -> AsyncResult<'b,'e>) : AsyncResult<'b,'e> = bind f x
  member this.Bind(x :Result<'a,'e>, f: 'a -> AsyncResult<'b,'e>): AsyncResult<'b,'e>  = resultAsyncBind f x 


let asyncResult = AsyncResultBuilder()


