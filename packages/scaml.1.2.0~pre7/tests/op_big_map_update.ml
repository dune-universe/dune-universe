[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  [], assert (
    BigMap.mem (Int 2) 
      (BigMap.update (Int 2) (Some (Int 2)) BigMap.empty)
  )

    
    

