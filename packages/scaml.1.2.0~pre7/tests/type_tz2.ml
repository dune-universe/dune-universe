[@@@SCaml iml_optimization=false]
open SCaml
(* XXX still need to check the mutez conversion is correct *)
let [@entry] main x y = 
  ([], 
   (* The maximum number allowed for tz *)
   assert (Tz 9223372036854.775807 = Tz 9223372036854.775807)
  )
    

