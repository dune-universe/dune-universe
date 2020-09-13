[@@@SCaml iml_optimization=false]
open SCaml
type point = { x : int ; y : int }

let [@entry] main x y = 
  [], assert (
    compare (Int 3, Int 3) (Int 3, Int 3) = Int 0
    && compare {x= Int 3; y= Int 3} {x= Int 3; y= Int 3} = Int 0
  )
    

