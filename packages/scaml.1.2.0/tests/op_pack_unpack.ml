[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert (match ( Obj.unpack (Obj.pack (Int 1)) : int option) with
      | None -> false
      | Some x -> x = Int 1)
