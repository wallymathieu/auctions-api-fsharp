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
type Amount={ value:float; currency:Currency }
type Auction={ id: AuctionId; title:string; endsAt:DateTime }
    with
        static member getId (auction:Auction)=
            auction.id
type Bid={ id:BidId; auction:Auction; at:DateTime; user:User; amount:Amount; retracted:DateTime option }
    with
        static member getId (bid:Bid)=
            bid.id

type Error=
    |UnknownUser of UserId
    |UnknownAuction of AuctionId
    |UnknownBid of BidId
    |BidAlreadyExists of BidId
    |AuctionAlreadyExists of AuctionId
    |AuctionHasEnded of AuctionId
    |SupportCantPlaceBids
    |CannotRemoveOtherPeoplesBids of UserId*BidId





                    
(*        member Save:User->Result<unit,Error>
        member Save:Bid->Result<unit,Error>
        member Save:Auction->Result<unit,Error>
        *)
    //new ConcurrentDictionary<long, Customer>()