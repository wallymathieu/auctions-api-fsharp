module Auctions.Option
let getOrElse d = 
   function
   | Some a -> a
   | None -> d