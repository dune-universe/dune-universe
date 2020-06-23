[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], 
  assert (
    Int 6 =
    Set.fold' (fun (n, sum) -> n + sum) (Set [ Int 1; Int 2; Int 3 ]) (Int 0) 
  )
    

