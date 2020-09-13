[@@@SCaml iml_optimization=false]
open SCaml
let main x y =
  [],
  assert ( Global.get_steps_to_quota () > Nat 0 )

