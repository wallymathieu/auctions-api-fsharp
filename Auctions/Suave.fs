module Suave
open System
open Suave
open Suave.Operators
open Suave.Successful
open Suave.Writers
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

let JSON v = 
  let jsonSerializerSettings = JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
  JsonConvert.SerializeObject(v, jsonSerializerSettings)
  |> OK
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
