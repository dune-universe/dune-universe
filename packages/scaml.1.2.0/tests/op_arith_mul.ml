[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [],
  assert (Int 2 * Int (-3) = Int (-6)
          && Nat 2 *^ Nat 3 = Nat 6
          && Tz 32.1 *$ Nat 3 = Tz 96.3) (* XXX failure cannot be tested *)

