[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (match Map.get (Int 2) (Map [ (Int 1, Nat 1); (Int 2, Nat 2) ]) with None -> false | Some x -> x = Nat 2)

