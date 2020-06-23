[@@@SCaml iml_optimization=false]
open SCaml
open Map
let [@entry] main x y = 
  [], assert (
    mem "a" (update "a" (Some "b") (Map ["b", "c"; "d", "e"]) )
  )
      

    
    

