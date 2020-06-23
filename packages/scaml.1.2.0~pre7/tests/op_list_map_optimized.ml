open SCaml
let [@entry] main () () = 
  [],
  assert ( List.length (List.map (fun x -> x = Int 1) [ Int 1; Int 2; Int 3 ]) = Nat 3)

