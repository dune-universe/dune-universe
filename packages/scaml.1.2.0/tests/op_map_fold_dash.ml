[@@@SCaml iml_optimization=false]
open SCaml
open Map
let [@entry] main () () =
  [], 
  assert (
    Int 21 =
    fold' (fun (k, v, sum) -> k + v + sum) 
      (Map [ Int 1, Int 2;
             Int 3, Int 4;
             Int 5, Int 6 ])
      (Int 0)
  )

