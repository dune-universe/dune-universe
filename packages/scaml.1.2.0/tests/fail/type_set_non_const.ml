[@@@SCaml iml_optimization=false]
open SCaml
let main x y = 
  [], 
  (* order of elements: Michelson does not care about it *)
  let x = Int 1 in
  assert (Set.length (Set [Int 3 ; Int 2; x]) = Nat 3)
                      
    

