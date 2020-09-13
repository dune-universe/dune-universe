(* MUST_FAIL *)
open SCaml

let main () () =
  [Operation.transfer_tokens (Int 1) (Tz 0.) Contract.self],
  ()
