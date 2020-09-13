(* INPUT= (Int 3, Int 1)
   STORAGE= Int 0
   
   tezos-client run script only performs 1 contract evaluation.
   It does not recurse.
*)
open SCaml
let [@entry] main (n, fact) st =
  match n with
  | Int 0 -> [], fact
  | _ -> 
      [Operation.transfer_tokens (n - Int 1, fact * n) (Tz 0.) Contract.self], st

