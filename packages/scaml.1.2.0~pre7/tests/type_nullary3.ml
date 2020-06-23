[@@@SCaml iml_optimization=false]
open SCaml
type t = A | B of int | C
let [@entry] main () _ = 
  match A with
  | A -> [], ()
  | B _x -> [], ()
  | C -> [], ()

            
