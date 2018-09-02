module Suave
open Suave
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Writers
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

module Json=
  module private Internals=
    let jsonSerializerSettings = JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()

    let stringify v=
      JsonConvert.SerializeObject(v, jsonSerializerSettings)
  open Internals
  let OK v : WebPart= 
    stringify v
    |> OK
    >=> setMimeType "application/json; charset=utf-8"
  let BAD_REQUEST v : WebPart= 
    stringify v
    |> BAD_REQUEST
    >=> setMimeType "application/json; charset=utf-8"

  let ``OK_or_BAD_REQUEST`` (result:Result<_,_>) : WebPart=
    match result with
    | Ok v -> OK v
    | Error err -> BAD_REQUEST err


  let getBody<'a> (ctx : HttpContext) = 
    let str = ctx.request.rawForm |> System.Text.Encoding.UTF8.GetString
    try 
      Ok(JsonConvert.DeserializeObject<'a> str)
    with exn -> Error exn
