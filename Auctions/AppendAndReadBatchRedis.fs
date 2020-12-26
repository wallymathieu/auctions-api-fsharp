namespace Auctions
open Auctions.Domain
open Auctions.MapToRedis

open StackExchange.Redis
open System.Collections.Generic
open System
open FSharpPlus

type AppendAndReadBatchRedis(connStr:string) =
  let conn = ConnectionMultiplexer.Connect(connStr)
  let db = conn.GetDatabase()
  let hashCreate (batch : IBatch) event =
    let id = Guid.NewGuid().ToString("N")
    let entries = Event.mapToHashEntries event
    batch.HashSetAsync(implicit id, entries |> List.toArray) |> ignore
    id

  let eventsKey:RedisKey = implicit "Events"
  interface IAppendBatch with

    member __.Batch events =
      let batch = db.CreateBatch()
      let ids = new List<RedisValue>()
      for events in events do
        let id = hashCreate batch events
        ids.Add(implicit (string id))
      batch.SetAddAsync(eventsKey, ids |> Seq.toArray, CommandFlags.None) |> ignore
      batch.Execute()
      async.Zero()

    member __.ReadAll() =
      let eventIdToEvent = string >> implicit >> db.HashGetAll >> List.ofArray >> Event.mapFromHashEntries
      let events = db.SetMembers(eventsKey, CommandFlags.None)
      events
      |> Array.map eventIdToEvent
      |> Array.toList
      |> List.sortBy Event.getAt
      |> async.Return