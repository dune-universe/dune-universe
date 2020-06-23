(* MUST_FAIL *)
(*
   INPUT=(fun x -> x + Int 1)
   STORAGE=Int 1
*)
(* void entrypoints in TZIP-4 https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md 

void a b = (a, lambda b b)

VOID :: void a b : S -> b : _
VOID code = UNPAIR; SWAP; DIP {code}; SWAP; EXEC;
            PUSH string "VoidResult"; PAIR; FAILWITH;
*)

open SCaml
    
let [@entry] [@entry] void (f : int -> int) (storage : int) =
  let res = f storage in
  failwith ("VoidResult", res)
