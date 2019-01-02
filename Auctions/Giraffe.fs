module Giraffe
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2
open System.IO

module Json =
  open FSharp.Data
  let inline ``OK_or_BAD_REQUEST`` (result:Result<_,_>) = fun next ctx ->
    (match result with
    | Ok v -> (setStatusCode 200 >=> json v)
    | Error err -> (setStatusCode 400 >=>json err)
    ) next ctx
  let inline getBody (ctx : HttpContext) = task {
    use reader = new StreamReader(ctx.Request.Body)
    let! body = reader.ReadToEndAsync()
    return try Ok (JsonValue.Parse body) with e -> Error <| string e
  }
