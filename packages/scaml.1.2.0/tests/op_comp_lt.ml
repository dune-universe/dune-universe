[@@@SCaml iml_optimization=false]
open SCaml
let v = Int 2
let w = Int 3
let [@entry] main x y = ([], assert (v < w))
    

