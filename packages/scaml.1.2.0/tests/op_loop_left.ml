[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [],
  Loop.left (fun x -> if x = Int 0 then Right () else Left (x - Int 1)) (Int 10)
