namespace Auctions
open Auctions.Domain

type IAppendBatch =
  abstract Batch : Event list -> Async<unit>
  abstract ReadAll : unit -> Async<Event list>
