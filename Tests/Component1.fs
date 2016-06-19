namespace Tests
open NUnit.Framework
open Domain
open System
[<TestFixture>]
type ``Can parse user`` ()=
    [<Test>] member test.
     ``Buyer or seller`` ()=
        let user = BuyerOrSeller(Guid.NewGuid(),"seller")
        in
            Assert.AreEqual(user, user.ToString() |> User.parse)

    [<Test>] member test.
     ``Support`` ()=
        let user = Support(Guid.NewGuid())
        in
            Assert.AreEqual(user, user.ToString() |> User.parse)
