module Commands
open Domain
open Auctions
open System
open Either


type Command =
    | Empty of at:DateTime
    | AddAuction of id:AuctionId *at:DateTime* title:string * endsAt:DateTime 
    | PlaceBid of auction:AuctionId* id: BidId* at: DateTime * amount:Amount* user:UserId
    | RemoveBid of id: BidId* user:UserId* at: DateTime 
    with
        /// the time when the command was issued
        static member getAt command=
            match command with
            | AddAuction(at=at) -> at
            | PlaceBid(at=at) -> at
            | RemoveBid(at=at) -> at
            | Empty(at=at) ->at

let handleCommand (r:Repository) command=
    match command with
    | Empty(at=at)-> Success()
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
                | Some _ ->
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
            | BuyerOrSeller(id=id;name=_)-> 
                if id <> User.getId bid.user then
                    return! Failure(CannotRemoveOtherPeoplesBids(id,bid.id))
                else
                    return! retract()
            | Support (id=_)->
                    return! retract()
        }
