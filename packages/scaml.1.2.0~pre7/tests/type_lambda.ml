[@@@SCaml iml_optimization=false]
(*
   STORAGE=(None : (int -> int) option)
*)
open SCaml
let [@entry] main () f =
  [], 
  Some (fun x -> (x : int))

