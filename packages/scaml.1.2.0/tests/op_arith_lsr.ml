[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], 
  assert ((Nat 9) lsr (Nat 3) = Nat 1)
