open Suave
//open Suave.Authentication
//open Suave.Cookie
open System
open Suave.Filters
open Suave.Model.Binding
open Suave.Operators
open Suave.RequestErrors
open Suave.State.CookieStateStore
open Suave.Successful
open Suave.Writers
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
type CmdArgs = { IP: System.Net.IPAddress; Port: Sockets.Port }
open Auctions.Domain
open Auctions.Actors
open Auctions

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

let JSON v =
  let jsonSerializerSettings = new JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

  JsonConvert.SerializeObject(v, jsonSerializerSettings)
  |> OK
  >=> Writers.setMimeType "application/json; charset=utf-8"

let getBodyAsJSON<'a> (req : HttpRequest) =
  let getString rawForm =
    System.Text.Encoding.UTF8.GetString(rawForm)
  req.rawForm |> getString |> JsonConvert.DeserializeObject<'a>

let overview r=
    GET >=> JSON (r |> Repo.auctions |> List.toArray)

let register r=
    POST >=> request (getBodyAsJSON<Auction> 
                      >> Timed.atNow 
                      >> Commands.AddAuction
                      >> (Commands.handleCommand r)
                      >> JSON)

let details r (id:AuctionId) =
    GET >=> JSON (Repo.getAuction r id)

let bids r (id:AuctionId)=
    GET >=> JSON (Repo.getAuctionBids r id)

let placeBid r (id:AuctionId)=
    POST >=> request (getBodyAsJSON<Bid> 
                      >> Timed.atNow 
                      >> Commands.PlaceBid
                      >> (Commands.handleCommand r)
                      >> JSON)

let webPart r= 
    choose [
        path "/" >=> (Successful.OK "")
        path Paths.Auction.overview >=> overview r
        path Paths.Auction.register >=> register r
        pathScan Paths.Auction.details (details r)
        pathScan Paths.Auction.bids (bids r)
        pathScan Paths.Auction.placeBid (placeBid r)
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
    let r = ConcurrentRepository()
    r |> Repo.saveAuction { 
                id=1L
                startsAt=DateTime(2011,1,1)
                title="Title"
                endsAt=DateTime(2012,1,1)
                user=User.BuyerOrSeller(Guid.NewGuid(), "Name") } |> ignore
    r |> Repo.saveAuction { 
                id=2L
                startsAt=DateTime(2011,1,1)
                title="Title2"
                endsAt=DateTime(2012,1,1)
                user=User.BuyerOrSeller(Guid.NewGuid(), "Name") } |> ignore

    // start suave
    startWebServer
        { defaultConfig with
            bindings = [ HttpBinding.create HTTP args.IP args.Port ] }
        (webPart r)
    0
