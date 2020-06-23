[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main _ _ = [], ()

(* bug
   PUSH (pair (list operation) unit) (Pair {} Unit)
*)

