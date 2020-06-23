[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert (Obj.pack "foobar" = Bytes "050100000006666f6f626172")
