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
open Auctions.Commands
open Newtonsoft.Json
module Paths = 
  type Int64Path = PrintfFormat<int64 -> string, unit, string, string, int64>
  
  module Auction = 
    /// /auctions
    let overview = "/auctions"
    /// /auction
    let register = "/auction"
    /// /auction/INT
    let details : Int64Path = "/auction/%d"
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
    currency : string
    [<JsonProperty("type")>]
    typ:string
}
type BidJsonResult = { 
      amount:Amount
      bidder:string
    }
type AuctionJsonResult = {
      id : AuctionId
      startsAt : DateTime
      title : string
      endsAt : DateTime
      currency : Currency
      bids : BidJsonResult array
    }

let webPart (agent : AuctionDelegator) = 

  let overview = 
    GET >=> fun (ctx) ->
            async {
              let! r = agent.GetAuctions()
              return! JSON (r |>List.toArray) ctx
            }

  let getAuctionResult id :Async<Result<AuctionJsonResult,Errors>>=
    let now =DateTime.UtcNow
    asyncResult{
      let! (a,bids) = async{
                        let! auctionAndBids = agent.GetAuction id
                        return match auctionAndBids with
                                | Some v-> Ok v
                                | None -> Error (UnknownAuction id)
                      }
      let discloseBidders =Auction.biddersAreOpen a
      let mapBid (b:Bid) :BidJsonResult = { 
        amount=b.amount
        bidder= if discloseBidders 
                then b.user.ToString() 
                else b.user.GetHashCode().ToString() // here you might want bidder number
      }
      let bids = match a.typ with
                 | English _ -> bids
                 // the bids are not disclosed until after the end :
                 | Vickrey -> if Auction.hasEnded now a then bids else []
                 | Blind -> if Auction.hasEnded now a then bids else [] 
      return { id=a.id; startsAt=a.startsAt; title=a.title;endsAt=a.endsAt; currency=a.currency
               bids=bids |> List.map mapBid |> List.toArray
             }
    }
  
  let details id  = GET >=> JSON(getAuctionResult id)

  /// handle command and add result to repository
  let handleCommandAsync (maybeC:Result<_,_>) = 
    asyncResult{
      let! c=maybeC
      return agent.UserCommand c
    }
  /// turn handle command to webpart
  let handleCommandAsync toCommand: WebPart = 
    fun (ctx : HttpContext) ->
      async {
        let r = toCommand ctx
        let! commandResult= handleCommandAsync r
        return! JSONorBAD commandResult ctx
      }

  let exnToInvalidUserData (err:exn)=InvalidUserData err.Message

  /// register auction
  let register = 

    let toPostedAuction user = 
      getBodyAsJSON<AddAuctionReq> 
        >> Result.map (fun a -> 
        let currency= Currency.tryParse a.currency |> Option.getOrElse Currency.VAC
        
        let typ = Type.tryParse a.typ |> Option.getOrElse (English { // default is english auctions
                                                                      reservePrice=Amount.zero currency 
                                                                      minRaise =Amount.zero currency
                                                                    })
        { user = user
          id=a.id
          startsAt=a.startsAt
          endsAt=a.endsAt
          title=a.title
          currency=currency
          typ=typ
        }
        |> Timed.atNow
        |> Commands.AddAuction)
        >> Result.mapError exnToInvalidUserData

    authenticated (function 
      | NoSession -> UNAUTHORIZED "Not logged in"
      | UserLoggedOn user -> 
        POST >=> handleCommandAsync (toPostedAuction user))

  /// place bid
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
           pathScan Paths.Auction.placeBid placeBid ]

[<EntryPoint>]
let main argv = 
  // parse arguments
  let args = 
    let (|Port|_|) = Parse.toTryParse System.UInt16.TryParse
    let (|IPAddress|_|) = Parse.toTryParse System.Net.IPAddress.TryParse
    
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
  
  let r = MutableRepository()
  let appenders = seq {
        if Option.isSome args.Redis then yield AppendAndReadBatchRedis(args.Redis.Value) :> IAppendBatch
        if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
      }
  let persist = PersistCommands (appenders |> Seq.map (fun a->a.Batch) |> List.ofSeq)
  let handleCommand c = handleCommand r c |> ignore
  for appender in appenders do
    appender.ReadAll() |> List.iter handleCommand

  persist.Start()
  let agent = createAgentDelegator(r, persist.Handle)
  // start suave
  startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP args.IP args.Port ] } (webPart agent)
  0
//curl  -X POST -d '{ "id":"1","startsAt":"2017-01-01","endsAt":"2018-01-01","title":"First auction" }' -H "x-fake-auth: BuyerOrSeller|a1|Seller"  -H "Content-Type: application/json"  127.0.0.1:8083/auction

//curl  -X POST -d '{ "auction":"1","amount":"VAC10" }' -H "x-fake-auth: BuyerOrSeller|a1|Test"  -H "Content-Type: application/json"  127.0.0.1:8083/auction/1/bid 