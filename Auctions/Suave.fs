module Suave
open System
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

let JSON v = 
  Json.stringify v
  |> OK
  >=> Writers.setMimeType "application/json; charset=utf-8"

let JSONorBAD (result:Result<_,_>)=
  match result with
  | Ok v -> JSON v
  | Error err -> 
    Json.stringify err
    |> BAD_REQUEST
    >=> Writers.setMimeType "application/json; charset=utf-8"


let getStringFromBytes rawForm = System.Text.Encoding.UTF8.GetString(rawForm)

let mapJsonPayload<'a> (req : HttpRequest) = 
  let fromJson json = 
    try 
      let obj = JsonConvert.DeserializeObject<'a>(json)
      Ok obj
    with e -> Error e
  req.rawForm
  |> getStringFromBytes
  |> fromJson

let getBodyAsJSON<'a> (req : HttpRequest) = 
  let str = req.rawForm |> getStringFromBytes
  try 
    Ok(JsonConvert.DeserializeObject<'a> str)
  with exn -> Error exn
