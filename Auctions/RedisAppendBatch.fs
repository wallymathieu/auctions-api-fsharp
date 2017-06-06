namespace Auctions

open StackExchange.Redis
open System.Collections.Generic
open System
open Commands
open MapToRedis

type AppendAndReadBatchRedis(db : IDatabase) = 
  
  let hashCreate (batch : IBatch) command = 
    let id = Guid.NewGuid().ToString("N")
    let entries = mapToHashEntries command
    batch.HashSetAsync(redisKey id, entries |> List.toArray) |> ignore
    id
  
  let commandsKey = redisKey "Commands"
  interface IAppendBatch with
    
    member this.Batch commands = 
      let batch = db.CreateBatch()
      let ids = new List<RedisValue>()
      for command in commands do
        let id = hashCreate batch command
        ids.Add(redisValueStr (id.ToString()))
      batch.SetAddAsync(commandsKey, ids |> Seq.toArray, CommandFlags.None) |> ignore
      batch.Execute()
    
    member this.ReadAll() = 
      let commands = db.SetMembers(commandsKey, CommandFlags.None)
      commands
      |> Array.map (fun key -> db.HashGetAll(valueToKey key))
      |> Array.map mapFromHashEntries
      |> Array.toList
      |> List.sortBy Command.getAt
