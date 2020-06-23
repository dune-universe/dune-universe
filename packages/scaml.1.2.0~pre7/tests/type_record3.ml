[@@@SCaml iml_optimization=false]
open SCaml

type t = { a : int ; b : int }

let [@entry] main () ()=
  let x = { a = Int 42; b = Int 43 } in
  [], ()
