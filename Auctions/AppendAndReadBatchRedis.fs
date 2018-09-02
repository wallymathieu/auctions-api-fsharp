namespace Auctions
open Auctions.Domain
open Auctions.MapToRedis

open StackExchange.Redis
open System.Collections.Generic
open System

type AppendAndReadBatchRedis(connStr:string) = 
  let conn = ConnectionMultiplexer.Connect(connStr)
  let db = conn.GetDatabase()
  let hashCreate (batch : IBatch) command = 
    let id = Guid.NewGuid().ToString("N")
    let entries = mapToHashEntries command
    batch.HashSetAsync(RedisKey.op_Implicit id, entries |> List.toArray) |> ignore
    id
  
  let commandsKey = RedisKey.op_Implicit "Commands"
  interface IAppendBatch with
    
    member __.Batch commands = 
      let batch = db.CreateBatch()
      let ids = new List<RedisValue>()
      for command in commands do
        let id = hashCreate batch command
        ids.Add(RedisValue.op_Implicit (string id))
      batch.SetAddAsync(commandsKey, ids |> Seq.toArray, CommandFlags.None) |> ignore
      batch.Execute()
    
    member __.ReadAll() = 
      let commands = db.SetMembers(commandsKey, CommandFlags.None)
      commands
      |> Array.map( (db.HashGetAll<<(string>>RedisKey.op_Implicit)) >> List.ofArray >> mapFromHashEntries )
      |> Array.toList
      |> List.sortBy Command.getAt
