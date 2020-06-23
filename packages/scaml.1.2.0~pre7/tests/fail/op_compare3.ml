(* Michelson code is rejected, since its comparable type definition around Pair has a bug.
   
   The code works if the record has 3 members.
*)
[@@@SCaml iml_optimization=false]
open SCaml
type point = { x : int ; y : int ; z : int ; w : int }

let [@entry] main x y = 
  [], assert (
    compare {x= Int 3; y= Int 3; z= Int 3; w= Int 3} 
            {x= Int 3; y= Int 3; z= Int 3; w= Int 3} = Int 0
  )
    

