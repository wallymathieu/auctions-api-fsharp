module Giraffe
open Giraffe
open HttpStatusCodeHandlers.Successful
open HttpStatusCodeHandlers.RequestErrors
open Microsoft.AspNetCore.Http

let ``JSONorBAD_REQUEST`` (result:Result<_,_>) = fun next ctx ->
  (match result with
  | Ok v -> json v |> OK
  | Error err -> json err |> BAD_REQUEST
  ) next ctx


let getBodyAsJSON<'a> (ctx : HttpContext) = 
  task {
    try 
      let! s=ctx.BindJsonAsync<'a>()
      return Ok s
    with exn -> return Error exn
  }
