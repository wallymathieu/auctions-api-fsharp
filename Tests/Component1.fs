namespace Tests
open NUnit.Framework
open FsUnit
open Commands

[<TestFixture>]
type ``Should map commands`` ()=
    [<Test>] member test.
     ``map add bid`` ()=
        11 |> should be (greaterThanOrEqualTo 10)