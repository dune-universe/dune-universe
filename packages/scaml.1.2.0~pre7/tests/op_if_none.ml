[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], 
  (* XXX evaluation rule of &&. Currently the second arg is evaluated first *) 
  assert 
    ((match Some (Int 1) with
        | None -> false
        | Some x -> x = Int 1)
     &&
     (match (None : tz option) with
      | None -> true
      | Some x -> false))



     
    

