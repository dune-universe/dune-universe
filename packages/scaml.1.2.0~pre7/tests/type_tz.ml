[@@@SCaml iml_optimization=false]
open SCaml
(* XXX still need to check the mutez conversion is correct *)
let [@entry] main x y = ([], assert (Tz 1.0 +$ Tz 0.000001 = Tz 1.000001))
    

