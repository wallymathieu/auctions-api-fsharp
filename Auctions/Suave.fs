module Suave
open Suave
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Writers
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

module Json=
  let jsonSerializerSettings = JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()

  let stringify v=
    JsonConvert.SerializeObject(v, jsonSerializerSettings)

let JSON v : WebPart= 
  Json.stringify v
  |> OK
  >=> setMimeType "application/json; charset=utf-8"

let ``JSONorBAD_REQUEST`` (result:Result<_,_>) : WebPart=
  match result with
  | Ok v -> JSON v
  | Error err -> 
    Json.stringify err
    |> BAD_REQUEST
    >=> setMimeType "application/json; charset=utf-8"

let private getStringFromBytes rawForm = System.Text.Encoding.UTF8.GetString(rawForm)

let getBodyAsJSON<'a> (ctx : HttpContext) = 
  let str = ctx.request.rawForm |> getStringFromBytes
  try 
    Ok(JsonConvert.DeserializeObject<'a> str)
  with exn -> Error exn
