[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y = 
  [], 
  let x = Int (-1) in
  assert (
    Set.length (Set [x ; Int 2; Int 3]) = Nat 3
    && Set.length (Set [] : int set) = Nat 0
  )

                      
    

