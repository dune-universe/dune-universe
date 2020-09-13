[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = ([], assert (Int (-1) + Int 2 = Int 1
                                   && Nat 1 +^ Nat 2 = Nat 3))
    

