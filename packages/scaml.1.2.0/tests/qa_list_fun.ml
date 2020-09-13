(*
  STORAGE=([] : (int -> int) list)
*)
(* Q: Can Michelson support list of functions?
   A: Yes
*)
open SCaml

let [@entry] main () st =
  [], (fun x -> x + Int 1) :: st
