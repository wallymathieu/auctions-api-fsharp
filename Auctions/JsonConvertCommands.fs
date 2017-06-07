module Auctions.JsonConvertCommands

open System
open Newtonsoft.Json
open System.Runtime.Serialization
open Commands

type ShortNameSerializationBinder(type' : Type) = 
  
  let types = 
    type'.Assembly.GetTypes()
    |> Array.filter (fun t -> type'.IsAssignableFrom(t))
    |> Array.map (fun t -> (t.Name, t))
    |> Map.ofArray
  
  interface Serialization.ISerializationBinder with
    
    member this.BindToName(serializedType, assemblyName, typeName) = 
      if (type'.IsAssignableFrom(serializedType)) then 
        assemblyName <- null
        typeName <- serializedType.Name
        ()
      else 
        assemblyName <- serializedType.Assembly.FullName
        typeName <- serializedType.FullName
        ()
    
    member this.BindToType(assemblyName, typeName) = 
      if (String.IsNullOrEmpty(assemblyName) && types.ContainsKey(typeName)) then types.[typeName]
      else Type.GetType(String.Format("{0}, {1}", typeName, assemblyName), true)

let _binder = new ShortNameSerializationBinder(typeof<Command>)
let settings = new JsonSerializerSettings()

settings.TypeNameHandling <- TypeNameHandling.Auto
settings.SerializationBinder <- _binder

let deserialize<'T> v = JsonConvert.DeserializeObject<'T>(v, settings)
let serialize obj = JsonConvert.SerializeObject(obj, settings)
