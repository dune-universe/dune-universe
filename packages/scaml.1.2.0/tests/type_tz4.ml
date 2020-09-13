(* MUST_FAIL *)
open SCaml
let [@entry] main x y = 
  let _ = if true then Tz 0.0 -$ Tz 1.0 else Tz 0.0 in (* must not be optimized out *)
  [], ()


