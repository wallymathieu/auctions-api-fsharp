module Domain
open System
type Currency=string
type UserId=Guid
type User = 
    | BuyerOrSeller of id:UserId * name:string
    | Support of id:UserId
    with
        static member getId user=
            match user with
            | BuyerOrSeller (id,_)->id
            | Support id->id

type BidId = Guid
type AuctionId = Guid
type Amount={ value:decimal; currency:Currency }
type Auction={ id: AuctionId; title:string; endsAt:DateTime }
    with
        static member getId (auction:Auction)=
            auction.id
type Bid={ id:BidId; auction:Auction; at:DateTime; user:User; amount:Amount; retracted:DateTime option }
    with
        static member getId (bid:Bid)=
            bid.id

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

type Error=
    |UnknownUser of UserId
    |UnknownAuction of AuctionId
    |UnknownBid of BidId
    |BidAlreadyExists of BidId
    |AuctionAlreadyExists of AuctionId
    |AuctionHasEnded of AuctionId
    |SupportCantPlaceBids
    |CannotRemoveOtherPeoplesBids of UserId*BidId


open System.Collections.Concurrent
type Repository()=
    // in memory repository
    let users= new ConcurrentDictionary<UserId, User>()
    let auctions= new ConcurrentDictionary<AuctionId, Auction>()
    let bids= new ConcurrentDictionary<BidId, Bid>()
    let auctionBids= new ConcurrentDictionary<AuctionId, ConcurrentBag<BidId>>()
    
    //inherit IRepository with
    member this.GetUser userId=
        match users.TryGetValue(userId) with
        | true,value->Success(value)
        | false,_->Failure(UnknownUser(userId))

    member this.GetAuction auctionId=
        match auctions.TryGetValue(auctionId) with
        | true,value->Success(value)
        | false,_->Failure(UnknownAuction(auctionId))

    member this.GetBid bidId=
        match bids.TryGetValue(bidId) with
        | true,value->Success(value)
        | false,_->Failure(UnknownBid(bidId))

    member this.TryGetBid bidId=
        match bids.TryGetValue(bidId) with
        | true,value->Some(value)
        | false,_->None

    member this.TryGetAuction auctionId=
        match auctions.TryGetValue(auctionId) with
        | true,value->Some(value)
        | false,_->None

    member this.SaveUser user=
        users.AddOrUpdate((User.getId user), user, (fun key oldvalue->user)) |> ignore
        Success()

    member this.SaveBid bid=
        let bidIds = auctionBids.AddOrUpdate(bid.auction.id, new ConcurrentBag<BidId>(), (fun key bag-> bag))
        bidIds.Add(bid.id) 
        bids.AddOrUpdate(Bid.getId bid, bid, (fun key oldvalue->bid)) |> ignore
        Success()

    member this.SaveAuction auction=
        auctions.AddOrUpdate(Auction.getId auction, auction, (fun key oldvalue->auction)) |> ignore
        Success()

    member this.GetBidsForAuction (auctionId:AuctionId)=
        match auctionBids.TryGetValue(auctionId) with
        | true, value -> value |> Seq.toList
        | false, _ -> []

type Command =
    | AddAuction of id:AuctionId * title:string * endsAt:DateTime
    | PlaceBid of auction:AuctionId* id: BidId* amount:Amount* user:UserId* at: DateTime
    | RemoveBid of id: BidId* user:UserId* at: DateTime 
//    | RemoveBidsFromAuction of auctionId: AuctionId * user:UserId* at: DateTime 


let handleCommand (r:Repository) command=
    match command with
    | AddAuction(id=id;title=title;endsAt=endsAt)->
        either{
            match r.TryGetAuction id with
            | Some _-> return! Failure(AuctionAlreadyExists id)
            | None -> return! r.SaveAuction {id=id;title=title;endsAt=endsAt}
        }
    | PlaceBid(id=id;auction=auction;amount=amount;user=user;at=at)->
        either{
            let! user = r.GetUser user
            let! auction = r.GetAuction auction
            let placeBid ()=
                match r.TryGetBid id with
                | None ->
                    if at > auction.endsAt then
                        Failure(AuctionHasEnded auction.id)
                    else
                        r.SaveBid {id=id;auction=auction;amount=amount;user=user;at=at;retracted=None}
                | Some bid ->
                    Failure(BidAlreadyExists id)

            match user with
            | BuyerOrSeller _ -> 
                return! placeBid()
            | Support _ -> 
                return! Failure( SupportCantPlaceBids )
        }

    | RemoveBid(id=id;user=user;at=at)->

        either{
            let! bid = r.GetBid id
            let! user = r.GetUser user
            let retract()=
                if at > bid.auction.endsAt then
                    Failure(AuctionHasEnded bid.auction.id)
                else
                    r.SaveBid {bid with retracted=Some(at)}


            match user with
            | BuyerOrSeller(id=id;name=name)-> 
                if id <> User.getId bid.user then
                    return! Failure(CannotRemoveOtherPeoplesBids(id,bid.id))
                else
                    return! retract()
            | Support (id=id)->
                    return! retract()
        }



                    
(*        member Save:User->Result<unit,Error>
        member Save:Bid->Result<unit,Error>
        member Save:Auction->Result<unit,Error>
        *)
    //new ConcurrentDictionary<long, Customer>()