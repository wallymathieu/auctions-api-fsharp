module Auctions.Jobs

open System.Threading
open System.Threading.Tasks
open Commands
open Domain
open Hopac
open Hopac.Infixes
open System.Collections.Generic

let create (results:Ch<Result<CommandSuccess,Errors>>) : Job<Ch<Command>> = job {
    let inCh = Ch ()
    let state = ConcurrentRepository()
    do! Job.foreverServer (inCh >>= fun msg-> 
         let maybeError = handleCommand state msg
                           |> Result.map snd
         results *<+ maybeError
    )
    return inCh
}