namespace Tests.Env
open Xunit
open Auctions.Environment

module ``Environment tests`` =
  [<Fact>]
  let ``can parse``() =
    let vars = [("AUCTIONS_IP","8081"); ("AUCTIONS_HOST","127.0.0.1"); ("AUCTIONS_web_hook", "10.20.2.8/webhook-receive")]
    let envArgs = vars |> Env.envArgs "AUCTIONS_"
    let expected = ["--ip";"8081"; "--host"; "127.0.0.1"; "--web-hook"; "10.20.2.8/webhook-receive"]
    Assert.Equal<string> (expected, envArgs)

  [<Fact>]
  let ``can ignore``() =
    let vars = [("APPSETTINGS_AUCTIONS__IP","8081");]
    let envArgs = vars |> Env.envArgs "AUCTIONS_"
    let expected = []
    Assert.Equal<string> (expected, envArgs)

