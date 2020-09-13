(* 
MUST_FAIL 
*)
open SCaml

exception Error of int * nat

let [@entry] main () () = raise (Error (Int 1, Nat 2))

  
