namespace Auctions

open Domain

type IAppendBatch = 
  abstract Batch : Command list -> unit
  abstract ReadAll : unit -> Command list
