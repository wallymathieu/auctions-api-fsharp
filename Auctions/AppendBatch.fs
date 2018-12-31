namespace Auctions
open Auctions.Domain

type IAppendBatch =
  abstract Batch : Command list -> Async<unit>
  abstract ReadAll : unit -> Async<Command list>
