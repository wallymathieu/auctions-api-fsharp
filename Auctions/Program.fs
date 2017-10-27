open System
open Suave
open Suave.Filters
open Suave.Operators
open Suave.RequestErrors
open Suave.Successful
open Suave.Writers

type CmdArgs = 
  { IP : System.Net.IPAddress
    Port : Sockets.Port
    Redis : string option
    Json : string option
  }

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

type BidReq = {amount : Amount}
type AddAuctionReq = {
    id : AuctionId
    startsAt : DateTime
    title : string
    endsAt : DateTime
}
let webPart (r:ConcurrentRepository) (agent : AuctionDelegator) = 
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

  /// handle command and add result to repository
  let handleCommandAsync (maybeC:Result<_,_>) = 
    asyncResult{
      let! c=maybeC
      let! r = agent.UserCommand c
      do addCommandResultToRepo r
      return r
    }

  let exnToInvalidUserData (err:exn)=InvalidUserData err.Message
  let handleCommandAsync toCommand: WebPart = 
    fun (ctx : HttpContext) ->
      async {
        let r = toCommand ctx
        let! commandResult= handleCommandAsync r
        return! JSONorBAD commandResult ctx
      }

  let register = 

    let toPostedAuction user = 
      getBodyAsJSON<AddAuctionReq> 
        >> Result.map (fun a -> 
        { user = user; id=a.id; startsAt=a.startsAt; endsAt=a.endsAt; title=a.title }
        |> Timed.atNow
        |> Commands.AddAuction)
        >> Result.mapError exnToInvalidUserData

    authenticated (function 
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user -> 
        POST >=> handleCommandAsync (toPostedAuction user))

  let placeBid (id : AuctionId) = 
    let toPostedPlaceBid user = 
      getBodyAsJSON<BidReq> 
        >> Result.map (fun a -> 
        let d = DateTime.UtcNow
        (d, 
         { user = user; id= BidId.NewGuid();
           amount=a.amount;auction=id
           at = d })
        |> Commands.PlaceBid)
        >> Result.mapError exnToInvalidUserData

    authenticated (function 
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user -> 
        POST >=> handleCommandAsync (toPostedPlaceBid user))
  
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
        Port = 8083us
        Redis = None
        Json = None
      }
    
    let rec parseArgs b args = 
      match args with
      | [] -> b
      | "--ip" :: IPAddress ip :: xs -> parseArgs { b with IP = ip } xs
      | "--port" :: Port p :: xs -> parseArgs { b with Port = p } xs
      | "--redis" :: conn :: xs -> parseArgs { b with Redis = Some conn } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
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
  let appenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadBatchRedis(args.Redis.Value) :> IAppendBatch
        if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
      }
  let persist = PersistCommands (appenders |> List.ofSeq)
  let handleCommand c = handleCommand r c |> ignore
  for appender in appenders do
    appender.ReadAll() |> List.iter handleCommand

  persist.Start()
  let agent = createAgentDelegator(r, persist.Handle)
  // start suave
  startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP args.IP args.Port ] } (webPart r agent)
  0
//curl  -X POST -d '{ "id":"1","startsAt":"2017-01-01","endsAt":"2018-01-01","title":"First auction" }' -H "x-fake-auth: BuyerOrSeller|a1|Seller"  -H "Content-Type: application/json"  127.0.0.1:8083/auction

//curl  -X POST -d '{ "auction":"1","amount":"VAC10" }' -H "x-fake-auth: BuyerOrSeller|a1|Test"  -H "Content-Type: application/json"  127.0.0.1:8083/auction/1/bid 