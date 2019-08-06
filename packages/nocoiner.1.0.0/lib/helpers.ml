module Int = Core.Int
module String = Core.String
module Char = Core.Char

let __nullchar = Char.of_int_exn 0

let pad ~basis msg =
  let encoded = Encoding.encode msg in
  let length = String.length encoded in
  let remainder = Int.( % ) length basis in
  let zerofill = String.make (basis - remainder) __nullchar in
  encoded ^ zerofill


let __nonzero char = char != __nullchar

let unpad msg = Encoding.decode @@ String.filter ~f:__nonzero msg
