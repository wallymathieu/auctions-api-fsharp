module Auctions.Security.Cryptography
open System
open System.Text
open System.Security.Cryptography

module SHA512=
  let ofString (str:string)=
    use sha = SHA512.Create()
    let bytes =
      str |> Encoding.UTF8.GetBytes
          |> sha.ComputeHash
    Convert.ToBase64String(bytes)
