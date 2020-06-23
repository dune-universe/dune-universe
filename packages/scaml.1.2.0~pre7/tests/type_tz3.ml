(* MUST_FAIL *)
open SCaml
let [@entry] main x y = ([], assert (Tz 0.0 -$ Tz 1.0 <> Tz 1.0))
    

