[@@@SCaml iml_optimization=false]
open SCaml

let f (x, y) = x + y

let [@entry] main () () =
  [], assert (f (Int 1, Int 2) = (Int 3))

               
