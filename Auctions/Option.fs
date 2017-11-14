[<AutoOpen>]
module Auctions.Options

[<RequireQualifiedAccess>]
module Option=
  let getOrElse d = 
     function
     | Some a -> a
     | None -> d


/// <summary>
/// </summary>
[<Sealed>]
type MaybeBuilder () =
    member inline __.Return
        value : 'T option =
        Some value

    member inline __.ReturnFrom
        value : 'T option =
        value

    member inline __.Zero () : 'T option = None
    member inline __.Delay (generator : unit -> 'T) = generator

    // The signature of Run corresponds closely to that of Delay.
    // This computation builder needs to define Run because Delay doesn't force evaluation
    // of the thunk it's given (it simply returns it).
    member __.Run (generator) : 'T option = generator ()

    member inline __.Bind
        (value, binder : 'T -> 'U option) : 'U option =
        Option.bind binder value

    member inline __.Combine
        (r1, r2 : 'T option) : 'T option =
        match r1 with
        | None ->
            None
        | Some () ->
            r2

[<CompiledName("Maybe")>]
let maybe = MaybeBuilder ()
