[@@@SCaml iml_optimization=false]
open SCaml
let v = Int 1
let [@entry] main x y = [], assert (v = Int 1)
