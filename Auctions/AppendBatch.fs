namespace Auctions
open Auctions.Domain

type IAppendBatch = 
  abstract Batch : Command list -> unit
  abstract ReadAll : unit -> Command list
