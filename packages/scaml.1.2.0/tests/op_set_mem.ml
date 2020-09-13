[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (Set.mem (Int 1) (Set [ Int 1; Int 2; Int 3 ]))
    

