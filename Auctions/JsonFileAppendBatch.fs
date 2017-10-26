namespace Auctions

open System.IO
open Commands
open System
open Newtonsoft.Json

type private ShortNameSerializationBinder(type' : Type) = 
  
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


type JsonAppendToFile(fileName) = 
  let _binder = new ShortNameSerializationBinder(typeof<Command>)
  let settings = new JsonSerializerSettings()
  
  do
    settings.TypeNameHandling <- TypeNameHandling.Auto
    settings.SerializationBinder <- _binder

  interface IAppendBatch with
    
    member this.Batch cs = 
      use fs = File.Open(fileName, FileMode.Append, FileAccess.Write, FileShare.Read)
      use w = new StreamWriter(fs)
      w.WriteLine(JsonConvert.SerializeObject (cs |> List.toArray), settings)
      fs.Flush()
    
    member this.ReadAll() = 
      use fs = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)
      use r = new StreamReader(fs)
      seq { 
        let line = ref ""
        
        let readLine() = 
          line := r.ReadLine()
          !line
        while (null <> readLine()) do
          yield JsonConvert.DeserializeObject<Command array>(!line, settings)
      }
      |> Seq.concat
      |> Seq.toList
      |> List.sortBy Command.getAt
