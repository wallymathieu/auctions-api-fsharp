namespace Auctions

open Commands

type IAppendBatch = 
  abstract Batch : Command list -> unit
  abstract ReadAll : unit -> Command list
