[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [],
  assert (match  String.slice (Nat 2) (Nat 3) "hello wolrd"  with
      | None -> false
      | Some x -> x = "llo"
    )
