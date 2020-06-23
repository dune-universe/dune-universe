(* file: lambdafail.ml *)
open SCaml

type func_t = (nat -> nat)
let f : func_t = fun _ -> (failwith "oops" : nat)
