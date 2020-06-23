(* MUST_FAIL *)
open SCaml

let [@entry] main () () =
  [], assert (Sum.get_left (Right (Int 1)) = Int 1)
    
