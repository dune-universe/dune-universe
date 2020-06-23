(* REJECT *)
[@@@SCaml iml_optimization=false]
open SCaml

let [@entry] main x y =
  [],
  assert (Obj.pack' (typerep_of_int [@scaml.replace assert false]) 
          = Obj.pack' (typerep_of_int [@scaml.replace assert false]) )
