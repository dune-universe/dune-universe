[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () =
  [],
  assert (
    Map.length (Map [ "a", "a"; "b", "b" ]) = Nat 2
  )
