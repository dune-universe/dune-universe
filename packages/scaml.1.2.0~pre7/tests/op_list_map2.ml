[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () = 
  [],
  let one = Int 1 in
  assert ( List.length (List.map (fun x -> x = one) [ Int 1; Int 2; Int 3 ]) = Nat 3)

