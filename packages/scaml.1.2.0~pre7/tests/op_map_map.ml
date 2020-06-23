[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (Map.length (Map.map (fun x y -> x+y) (Map [ (Int 1, Int 1); (Int 2, Int 2) ])) = Nat 2)

