[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], 
  assert (
    false = 
    Set.fold (fun n sum -> n = Int 0 || sum) (Set [ Int 1; Int 2; Int 3 ]) false
  )
    

