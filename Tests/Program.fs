open System.Globalization
module Program = 
    let [<EntryPoint>] main _ = 
        CultureInfo.DefaultThreadCurrentCulture <- CultureInfo.InvariantCulture
        CultureInfo.DefaultThreadCurrentUICulture <- CultureInfo.InvariantCulture
        0
