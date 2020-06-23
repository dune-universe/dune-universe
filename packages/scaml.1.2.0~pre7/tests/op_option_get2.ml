(* MUST_FAIL *)
open SCaml

let [@entry] main () () =
  [], assert (Option.get None = Int 1)
