module Auctions.Parse

/// turn a .net try parse into a f# try parse
let toTryParse f str = 
  match f str with
  | (true, i) -> Some i
  | _ -> None