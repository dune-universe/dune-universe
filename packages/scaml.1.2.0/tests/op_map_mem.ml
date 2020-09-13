[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (Map.mem (Int 2) (Map [ (Int 1, Int 1); (Int 2, Int 2) ]))



