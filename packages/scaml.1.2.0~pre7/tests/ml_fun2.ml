[@@@SCaml iml_optimization=false]
open SCaml

let f x y = x - y

let [@entry] main x y = [], assert (f (Int 7) (Int 4) = Int 3)
