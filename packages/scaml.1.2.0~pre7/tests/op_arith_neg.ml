[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [],
  assert (
    ~- (Int (-2)) = Int 2 
    && ~-^ (Nat 2) = Int (-2)
  )

