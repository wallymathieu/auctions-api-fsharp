[<AutoOpen>]
module Auctions.Results

type ResultBuilder() = 
  member __.Bind(x, f) = Result.bind f x
  member __.ReturnFrom x = x
  member __.Zero() = Result.Ok()
  member __.Return(x) = Result.Ok(x)

  member __.Delay f= f()

let result = ResultBuilder()

//from https://github.com/SuaveIO/suave/blob/master/src/Suave/WebPart.fs
type AsyncResult<'a,'e> = Async<Result< 'a,'e>>

type AsyncResultBuilder() =
  let bind (f: 'a -> AsyncResult<'b,'e>) (a: AsyncResult<'a,'e>) = async {
    let! p = a
    match p with
    | Error e ->
      return Error e
    | Ok q ->
      let r = f q
      return! r
    }
  let resultAsyncBind (f: 'a -> AsyncResult<'b,'e>) (a: Result<'a,'e>) = async {
    match a with
    | Error e ->
      return Error e
    | Ok q ->
      let r = f q
      return! r
    }
  member __.Return(x:'a) : AsyncResult<'a,'e> = async { return Ok x }
  member __.Zero() : AsyncResult<unit,'e> = async { return Ok () }
  member __.ReturnFrom(x : AsyncResult<'a,'e>) = x
  member __.Delay(f: unit ->  AsyncResult<'a,'e>) = async { return! f () }
  member __.Bind(x :AsyncResult<'a,'e>, f : 'a -> AsyncResult<'b,'e>) : AsyncResult<'b,'e> = bind f x
  member __.Bind(x :Result<'a,'e>, f: 'a -> AsyncResult<'b,'e>): AsyncResult<'b,'e>  = resultAsyncBind f x 


let asyncResult = AsyncResultBuilder()

