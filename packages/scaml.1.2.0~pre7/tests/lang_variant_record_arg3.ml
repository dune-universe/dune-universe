[@@@SCaml iml_optimization=false]
open SCaml

type t = A of { x : nat ; y : int } | B

let [@entry] main () _ = 
  match A {x = Nat 1; y = Int 1 } with
  | A { x; y } -> [], assert (abs y = x)
  | B -> assert false
    
