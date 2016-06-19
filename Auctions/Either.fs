module Either

type Result<'TSuccess,'TFailure>=
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind f x = 
    match x with
    | Success s -> f s
    | Failure f -> Failure f
type EitherBuilder () =
    member this.Bind(x, f) = bind f x
    member this.ReturnFrom x = x
    member this.Zero() =Success ()
let either = new EitherBuilder()

