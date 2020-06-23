[@@@SCaml iml_optimization=false]
open SCaml

let [@entry] main x y =
  [],
  assert (match 
            ( Obj.unpack' (typerep_of_int [@scaml.replace assert false])
                (Obj.pack' (typerep_of_int [@scaml.replace assert false]) (Int 1)) : int option) with
      | None -> false
      | Some x -> x = Int 1)
