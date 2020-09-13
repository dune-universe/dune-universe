[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (Set.length (Set.empty : int set) = Nat 0)

