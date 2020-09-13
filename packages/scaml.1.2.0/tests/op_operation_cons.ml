(* [@@@SCaml iml_optimization=false] cannot disable optimization,
   since it keeps closures with operation
*)
open SCaml
let [@entry] main x y = 
  let os : operation list = [] in
  let o = 
    if true then Operation.set_delegate None
    else Operation.set_delegate None
  in
  o::os, ()
