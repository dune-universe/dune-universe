[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], 
  (* ordering of keys: Michelson cares it. *)
  assert (Map.length (Map [("Yes", Int 1); ("No", Int 2)]) = Nat 2)

                      
    

