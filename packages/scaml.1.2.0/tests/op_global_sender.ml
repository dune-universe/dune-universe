[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert ( Global.get_sender () = Global.get_sender ())
