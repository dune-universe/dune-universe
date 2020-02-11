open Expr12
open Expr08
open Expr08double

let econst e = h (EConst e)
let eadd e1 e2 = h (EAdd (e1, e2))

let e1 : hexpr = eadd (econst 0) (econst 1)
let e2 : hexpr = new hmap # visit_'expr () e1 (* identity *)
let () =
  Printf.printf "%b\n%!" (e1 == e2) (* should print true *)
let e3 : hexpr = eadd (econst 0) (econst 2)
let e4 : hexpr = double e1 (* should produce [e3] *)
let () =
  Printf.printf "%b\n%!" (e3 == e4) (* should print true *)
