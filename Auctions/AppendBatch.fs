namespace Auctions
open Commands

type IAppendBatch =
    abstract member Batch: Command list->unit
    abstract member ReadAll: unit->Command list

