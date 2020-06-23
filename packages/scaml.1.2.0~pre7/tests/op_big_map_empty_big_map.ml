[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (BigMap.mem (Int 1) (BigMap.empty : (int, nat) big_map) = false)
    

