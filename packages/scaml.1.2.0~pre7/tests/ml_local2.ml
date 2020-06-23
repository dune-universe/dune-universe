[@@@SCaml iml_optimization=false]
open SCaml
let v = Int 3
let w = Int 10
let [@entry] main x y = [], assert (w - v = Int 7)
