namespace Auctions
open Auctions.Domain

open System.IO
open System
open Newtonsoft.Json

type private ShortNameSerializationBinder(type' : Type) = 
  
  let types = 
    type'.Assembly.GetTypes()
    |> Array.filter (fun t -> type'.IsAssignableFrom(t))
    |> Array.map (fun t -> (t.Name, t))
    |> Map.ofArray
  
  interface Serialization.ISerializationBinder with
    
    member __.BindToName(serializedType, assemblyName, typeName) = 
      if (type'.IsAssignableFrom(serializedType)) then 
        assemblyName <- null
        typeName <- serializedType.Name
        ()
      else 
        assemblyName <- serializedType.Assembly.FullName
        typeName <- serializedType.FullName
        ()
    
    member __.BindToType(assemblyName, typeName) = 
      if (String.IsNullOrEmpty(assemblyName) && types.ContainsKey(typeName)) then types.[typeName]
      else Type.GetType(String.Format("{0}, {1}", typeName, assemblyName), true)


type JsonAppendToFile(fileName) = 
  let _binder = ShortNameSerializationBinder(typeof<Command>)
  let settings = JsonSerializerSettings()
  let notNull= not << isNull
  
  do
    if not <| File.Exists fileName then File.WriteAllText(fileName, "")
    else 
        ()

    settings.TypeNameHandling <- TypeNameHandling.Auto
    settings.SerializationBinder <- _binder

  interface IAppendBatch with
    
    member __.Batch cs = 
      use fs = File.Open(fileName, FileMode.Append, FileAccess.Write, FileShare.Read)
      use w = new StreamWriter(fs)
      let json = JsonConvert.SerializeObject(List.toArray cs, settings)
      w.WriteLine json
      fs.Flush()
    
    member __.ReadAll() = 
      use fs = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)
      use r = new StreamReader(fs)
      seq { 
        let line = ref ""
        
        let readLine() = 
          line := r.ReadLine()
          line.Value
        while (notNull <| readLine()) do
          yield JsonConvert.DeserializeObject<Command array>(line.Value, settings)
      }
      |> Seq.concat
      |> Seq.toList
      |> List.sortBy Command.getAt
