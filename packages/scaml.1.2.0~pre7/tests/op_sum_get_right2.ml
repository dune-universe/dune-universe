(* MUST_FAIL *)
open SCaml

let [@entry] main () () =
  [], assert (Sum.get_right (Left (Int 1)) = Int 1)
    
