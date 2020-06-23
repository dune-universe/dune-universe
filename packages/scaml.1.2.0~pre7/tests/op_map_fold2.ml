[@@@SCaml iml_optimization=false]
open SCaml
open Map
let [@entry] main () () =
  [], 
  assert (
    false =
    fold (fun k v sum -> sum || abs k +^ v = Nat 0) 
      (Map [ Int 1, Nat 2;
             Int 3, Nat 4;
             Int 5, Nat 6 ])
      false
  )

