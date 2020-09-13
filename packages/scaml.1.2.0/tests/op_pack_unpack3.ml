(* REJECT *)
[@@@SCaml iml_optimization=false]
open SCaml

let [@entry] main x y =
  [],
  assert (Obj.TypeSafe.pack (typerep_of_int [@scaml.replace assert false])
          = Obj.TypeSafe.pack (typerep_of_int [@scaml.replace assert false]) )
