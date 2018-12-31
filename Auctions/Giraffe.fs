﻿module Giraffe
open Giraffe
open HttpStatusCodeHandlers.Successful
open HttpStatusCodeHandlers.RequestErrors
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2
open System.IO
open Fleece.FSharpData
let inline json dataObj : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        ctx.SetContentType "application/json; charset=utf-8"
        toJson dataObj
        |> string
        |> ctx.WriteStringAsync
let inline ``JSONorBAD_REQUEST`` (result:Result<_,_>) = fun next ctx ->
  (match result with
  | Ok v ->  json v |> OK
  | Error err -> json err |> BAD_REQUEST
  ) next ctx


let inline getBodyAsJSON (ctx : HttpContext) = task {
    try
      use reader = new StreamReader(ctx.Request.Body)
      let! body = reader.ReadToEndAsync()
      return parseJson(body)
    with exn -> return Error <| string exn
  }
