(* [@@@SCaml iml_optimization=false] 
   We must set optimization on, otherwise a contract is passed to APPLY,
   which is rejected saying "contract type forbidden in storage and constants"
*)
open SCaml
let [@entry] main x y = 
  [ Operation.transfer_tokens () (Tz 10.) Contract.self
  ; Operation.transfer_tokens () (Tz 11.) Contract.self
  ],
  ()

