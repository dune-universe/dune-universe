[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [],
  assert (Int 1 - Int 3 = Int (-2)
          && Nat 1 -^ Nat 3 = Int (-2)
          && Tz 32.1 -$ Tz 1.23 = Tz 30.87) (* XXX failure cannot be tested *)

