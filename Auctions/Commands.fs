module Commands
open Domain
open Auctions
open System
open Either

type Command =
    | AddAuction of id:AuctionId * title:string * endsAt:DateTime
    | PlaceBid of auction:AuctionId* id: BidId* amount:Amount* user:UserId* at: DateTime
    | RemoveBid of id: BidId* user:UserId* at: DateTime 


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
