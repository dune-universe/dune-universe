[@@@SCaml iml_optimization=false]
(*
   STORAGE=(None : (int -> int) option)
*)
open SCaml
let [@entry] main () f =
  [],
  let y = Int 3 in
  Some (fun (x : int) -> y)

