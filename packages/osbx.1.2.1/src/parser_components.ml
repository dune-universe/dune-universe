open Angstrom

let integer64 =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int64.of_string
;;
