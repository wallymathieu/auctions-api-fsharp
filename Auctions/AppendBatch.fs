namespace Auctions
open Auctions.Domain

type IAppendBatch<'T> =
  abstract Batch : 'T list -> Async<unit>
  abstract ReadAll : unit -> Async<'T list>
