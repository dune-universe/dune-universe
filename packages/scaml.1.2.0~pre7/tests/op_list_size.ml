[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () = 
  [],
  assert ( List.length [ Int 1; Int 2; Int 3 ] = Nat 3 )
