[@@@SCaml iml_optimization=false, protocol=7.0]
open SCaml
let [@entry] main x y =
  [],
  assert (Global.get_level () >= Nat 0)
