module Tests.StringTests

open Xunit
open Auctions.Web

module ``String tests`` =
  [<Fact>]
  let ``can get string after Bearer``() =
    match "Bearer 13213" with | Bearer token->Assert.Equal ("13213", token) | _ -> failwith "!"
  [<Fact>]
  let ``will return nothing when Bearer is not a prefix``() =
    match "Something 13213" with | Bearer _ -> failwith "!" | _ -> ()
