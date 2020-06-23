[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (
    compare (Int 3) (Int 3) = Int 0
    && compare (Nat 3) (Nat 2) = Int 1
    && compare (Tz 1.2) (Tz 10.2) = Int (-1)
  )
    

