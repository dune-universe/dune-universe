[@@@SCaml iml_optimization=false]
open SCaml

type t = A of { x: nat }

let [@entry] main () _ = 
  match A {x = Nat 0} with
  | A _ -> [], ()
