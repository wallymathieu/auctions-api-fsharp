module Tests.AuctionStateSpecs
open Auctions.Domain
open Xunit
[<AbstractClass>]
type IncrementSpec(baseState) = 

  [<Fact>]
  member __.``can increment twice 1``() = 
    let s= S.inc endsAt baseState in
    let s2= S.inc endsAt s in
    Assert.Equal(s, s2)

  [<Fact>]
  member __.``can increment twice 2``() = 
    let s= S.inc startsAt baseState in
    let s2= S.inc startsAt s in
    Assert.Equal(s, s2)

  [<Fact>]
  member __.``wont end just after start``() = 
    let state = baseState |> S.inc (startsAt.AddHours(1.0))
    Assert.False(S.hasEnded state)

  [<Fact>]
  member __.``wont end just before start``() = 
    let state = baseState |> S.inc (startsAt.AddHours(-1.0))
    Assert.False(S.hasEnded state)

  [<Fact>]
  member __.``wont end just before end``() = 
    let state = baseState |> S.inc (endsAt.AddHours(-1.0))
    Assert.False(S.hasEnded state)

  [<Fact>]
  member __.``will have ended just after end``() = 
    let state = baseState |> S.inc (endsAt.AddHours(1.0))
    Assert.True(S.hasEnded state)


