open System
open Suave
open Suave.Filters
open Suave.Model.Binding
open Suave.Operators
open Suave.RequestErrors
open Suave.State.CookieStateStore
open Suave.Successful
open Suave.Writers
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

type CmdArgs = 
  { IP : System.Net.IPAddress
    Port : Sockets.Port }

open Auctions.Domain
open Auctions.Actors
open Auctions
open Auctions.Either
open Auctions.Commands

module Paths = 
  type Int64Path = PrintfFormat<int64 -> string, unit, string, string, int64>
  
  module Auction = 
    /// /auctions
    let overview = "/auctions"
    /// /auction
    let register = "/auction"
    /// /auction/INT
    let details : Int64Path = "/auction/%d"
    /// /auction/INT/bids
    let bids : Int64Path = "/auction/%d/bids"
    /// /auction/INT/bid
    let placeBid : Int64Path = "/auction/%d/bid"

type Session = 
  | NoSession
  | UserLoggedOn of User

let authenticated f = 
  context (fun x -> 
    match x.request.header "x-fake-auth" with
    | Choice1Of2 u -> 
        User.tryParse u |> function | Some user->
                                        f (UserLoggedOn(user))
                                    | None ->f NoSession
    | Choice2Of2 _ -> f NoSession)

let JSON v = 
  let jsonSerializerSettings = JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
  JsonConvert.SerializeObject(v, jsonSerializerSettings)
  |> OK
  >=> Writers.setMimeType "application/json; charset=utf-8"

let getStringFromBytes rawForm = System.Text.Encoding.UTF8.GetString(rawForm)

let mapJsonPayload<'a> (req : HttpRequest) = 
  let fromJson json = 
    try 
      let obj = JsonConvert.DeserializeObject<'a>(json)
      Ok obj
    with e -> Error e
  req.rawForm
  |> getStringFromBytes
  |> fromJson

let getBodyAsJSON<'a> (req : HttpRequest) = 
  let str = req.rawForm |> getStringFromBytes
  try 
    Ok(JsonConvert.DeserializeObject<'a> str)
  with exn -> Error (InvalidUserData exn.Message)
type BidReq = {amount : Amount}
type AddAuctionReq = {
    id : AuctionId
    startsAt : DateTime
    title : string
    endsAt : DateTime
}
let webPart (r:ConcurrentRepository) (agent : Agent<DelegatorSignals>) = 
  let overview = 
    GET >=> JSON(r
                 |> Repo.auctions
                 |> List.toArray)
  
  let details (id : AuctionId) = GET >=> JSON(Repo.getAuction r id)
  let bids (id : AuctionId) = GET >=> JSON(Repo.getAuctionBids r id)
  
  let addCommandResultToRepo result = 
    match result with
    | AuctionAdded(t, a) -> r |> Repo.saveAuction a
    | BidAccepted(t, b) -> r |> Repo.saveBid b
    |> ignore
  
  let handleCommand maybeC = 
    either { 
      let! c = maybeC
      let! result = agent.PostAndReply(fun r -> UserCommand(c, r))
      do addCommandResultToRepo result
      return result
    }
  
  let register = 
    let toPostedAuction user = 
      getBodyAsJSON<AddAuctionReq> >> Result.map (fun a -> 
                                  { user = user; id=a.id; startsAt=a.startsAt; endsAt=a.endsAt; title=a.title }
                                  |> Timed.atNow
                                  |> Commands.AddAuction)
    authenticated (function 
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user -> 
        POST >=> request (toPostedAuction user
                          >> handleCommand
                          >> JSON))
  let placeBid (id : AuctionId) = 
    let toPostedPlaceBid user = 
      getBodyAsJSON<BidReq> >> Result.map (fun a -> 
                              let d = DateTime.UtcNow
                              (d, 
                               { user = user; id= BidId.NewGuid();
                                 amount=a.amount;auction=id
                                 at = d })
                              |> Commands.PlaceBid)
    authenticated (function 
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user -> 
        POST >=> request (toPostedPlaceBid user
                          >> handleCommand
                          >> JSON))
  
  choose [ path "/" >=> (Successful.OK "")
           path Paths.Auction.overview >=> overview
           path Paths.Auction.register >=> register
           pathScan Paths.Auction.details details
           pathScan Paths.Auction.bids bids
           pathScan Paths.Auction.placeBid placeBid ]

[<EntryPoint>]
let main argv = 
  // parse arguments
  let args = 
    let parse f str = 
      match f str with
      | (true, i) -> Some i
      | _ -> None
    
    let (|Port|_|) = parse System.UInt16.TryParse
    let (|IPAddress|_|) = parse System.Net.IPAddress.TryParse
    
    //default bind to 127.0.0.1:8083
    let defaultArgs = 
      { IP = System.Net.IPAddress.Loopback
        Port = 8083us }
    
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
    
    argv
    |> List.ofArray
    |> parseArgs defaultArgs
  
  let r = ConcurrentRepository()
  r
  |> Repo.saveAuction { id = 1L
                        startsAt = DateTime(2011, 1, 1)
                        title = "Title"
                        endsAt = DateTime(2022, 1, 1)
                        user = User.BuyerOrSeller(Guid.NewGuid().ToString("N"), "Name") }
  |> ignore
  r
  |> Repo.saveAuction { id = 2L
                        startsAt = DateTime(2011, 1, 1)
                        title = "Title2"
                        endsAt = DateTime(2022, 1, 1)
                        user = User.BuyerOrSeller(Guid.NewGuid().ToString("N"), "Name") }
  |> ignore
  let agent = createAgentDelegator r
  // start suave
  startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP args.IP args.Port ] } (webPart r agent)
  0
//curl  -X POST -d '{ "auction":"1","amount":"VAC10" }' -H "x-fake-auth: BuyerOrSeller|a1|Test"  -H "Content-Type: application/json"  127.0.0.1:8083/auction/1/bid 