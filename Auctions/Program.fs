open Suave
//open Suave.Authentication
//open Suave.Cookie
open Suave.Filters
open Suave.Model.Binding
open Suave.Operators
open Suave.RequestErrors
open Suave.State.CookieStateStore
open Suave.Successful

type CmdArgs = { IP: System.Net.IPAddress; Port: Sockets.Port }
open Auctions.Domain
open Auctions.Actors

module Paths=
    type Int64Path = PrintfFormat<(int64 -> string),unit,string,string,int64>

    module Auction =
        let overview = "/auctions"
        let register = "/auction"
        let details : Int64Path = "/auction/%d"
        let bids : Int64Path = "/auction/%d/bids"
        let placeBid : Int64Path = "/auction/%d/bid"


type Session = 
    | NoSession
    | UserLoggedOn of User

let session f = 
    statefulForSession
    >=> context (fun x -> 
        match x |> HttpContext.state with
        | None -> f NoSession
        | Some state ->
            match state.get "user" with
            | Some user -> f (UserLoggedOn (User.parse user))
            | _ -> f NoSession)

let overview =
    choose [
        GET >=> Successful.OK ""
    ]

let register=
    choose [
        GET >=> Successful.OK ""
    ]

let details (id:AuctionId)=
    choose [
        GET >=> Successful.OK ""
    ]

let bids (id:AuctionId)=
    choose [
        GET >=> Successful.OK ""
    ]
let placeBid (id:AuctionId)=
    choose [
        GET >=> Successful.OK ""
    ]

let webPart = 
    choose [
        path "/" >=> (Successful.OK "Hello World!")
        path Paths.Auction.overview >=> overview
        path Paths.Auction.register >=> register
        pathScan Paths.Auction.details details
        pathScan Paths.Auction.bids bids
        pathScan Paths.Auction.placeBid placeBid
    ]
[<EntryPoint>]

let main argv = 

    // parse arguments
    let args =
        let parse f str = match f str with (true, i) -> Some i | _ -> None

        let (|Port|_|) = parse System.UInt16.TryParse
        let (|IPAddress|_|) = parse System.Net.IPAddress.TryParse

        //default bind to 127.0.0.1:8083
        let defaultArgs = { IP = System.Net.IPAddress.Loopback; Port = 8083us }

        let rec parseArgs b args =
            match args with
            | [] -> b
            | "--ip" :: IPAddress ip :: xs -> parseArgs { b with IP = ip } xs
            | "--port" :: Port p :: xs -> parseArgs { b with Port = p } xs
            | invalidArgs ->
                printfn "error: invalid arguments %A" invalidArgs
                printfn "Usage:"
                printfn "    --ip ADDRESS   ip address (Default: %O)" defaultArgs.IP
                printfn "    --port PORT    port (Default: %i)" defaultArgs.Port
                exit 1

        argv |> List.ofArray |> parseArgs defaultArgs

    // start suave
    startWebServer
        { defaultConfig with
            bindings = [ HttpBinding.create HTTP args.IP args.Port ] }
        webPart
    0
