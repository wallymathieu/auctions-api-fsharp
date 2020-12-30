namespace Auctions
open Auctions.Domain
open Auctions.MapToRedis

open StackExchange.Redis
open System
open FSharpPlus

module AppendAndReadBatchRedis =
  let hashCreate mapToHashEntries (batch : IBatch) event =
    let id = Guid.NewGuid().ToString("N")
    let entries = mapToHashEntries event
    batch.HashSetAsync(implicit id, entries |> List.toArray) |> ignore
    id
  let batch (db:IDatabase) mapToHashEntries key entities =
      let batch = db.CreateBatch()
      let ids = ResizeArray<RedisValue>()
      for entity in entities do
        let id = hashCreate mapToHashEntries batch entity
        ids.Add(implicit (string id))
      batch.SetAddAsync(key, ids |> Seq.toArray, CommandFlags.None) |> ignore
      batch.Execute()
      async.Zero()
  let readAll (db:IDatabase) mapFromHashEntries getAt key =
      let entityIdToEntity = string >> implicit >> db.HashGetAll >> List.ofArray >> mapFromHashEntries
      let entities = db.SetMembers(key, CommandFlags.None)
      entities
      |> Array.map entityIdToEntity
      |> Array.toList
      |> List.sortBy getAt
      |> async.Return

type AppendAndReadBatchRedis<'T>(connStr:string, entitiesKey:string, mapToHashEntries, mapFromHashEntries, getAt) =
  let conn = ConnectionMultiplexer.Connect(connStr)
  let db = conn.GetDatabase()

  let entitiesKey:RedisKey = implicit entitiesKey
  interface IAppendBatch<'T> with

    member __.Batch events = AppendAndReadBatchRedis.batch db mapToHashEntries entitiesKey events

    member __.ReadAll() = AppendAndReadBatchRedis.readAll db mapFromHashEntries getAt entitiesKey

type AppendAndReadEventBatchRedis(connStr:string) =
  inherit AppendAndReadBatchRedis<Event>(connStr, "Events", Event.mapToHashEntries, Event.mapFromHashEntries, Event.getAt)

type AppendAndReadCommandBatchRedis(connStr:string) =
  inherit AppendAndReadBatchRedis<Command>(connStr, "Commands", Command.mapToHashEntries, Command.mapFromHashEntries, Command.getAt)