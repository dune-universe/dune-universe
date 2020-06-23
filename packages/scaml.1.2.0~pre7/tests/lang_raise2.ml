(* 
MUST_FAIL 
*)
open SCaml

let [@entry] main () () = raise (Failure "hello")



  
