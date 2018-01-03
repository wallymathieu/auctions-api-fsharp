[<AutoOpen>]
module Auctions.Options

/// <summary>
/// </summary>
[<Sealed>]
type MaybeBuilder () =
    member inline __.Return value  = Some value
    member inline __.ReturnFrom value = value
    member inline __.Zero () = None
    member inline __.Bind(value, binder) = Option.bind binder value


[<CompiledName("Maybe")>]
let maybe = MaybeBuilder ()
