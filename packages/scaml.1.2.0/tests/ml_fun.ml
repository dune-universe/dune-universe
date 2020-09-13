[@@@SCaml iml_optimization=false]
open SCaml

let f x = x + Int 2

let [@entry] main x y = [], assert (f (Int 3) = Int 5)
