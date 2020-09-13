[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () = 
  [],
  assert ( List.fold_left (fun acc x -> acc || x = Int 0) false [ Int 1; Int 2; Int 3 ] = false )

