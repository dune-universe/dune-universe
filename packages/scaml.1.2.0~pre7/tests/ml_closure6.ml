(* closure test *)
[@@@SCaml iml_optimization=false]
open SCaml

let a = Int 1

let f c = (* this should create a closure with [a] *)
  let d = c + a in
  let g e = e + d in (* another closure with [d] *)
  g

let [@entry] main x y = [], assert (f (Int 3) (Int 2) = Int 6)
