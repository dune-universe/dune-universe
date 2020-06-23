[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = ([], assert (fst (Int 1, Int 2) = snd (Int 2, Int 1)))
    

