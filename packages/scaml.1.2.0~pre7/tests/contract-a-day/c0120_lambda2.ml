(* ../test.sh c120_lambda2.ml *)
open SCaml

let main () ads =
  (* Too bad, we have no way to put OCaml code to produce new contracts ... *)
  let op, ad =
    Contract.create_from_tz_file "_build/c0120_lambda.tz" None (Tz 0.0) ()
  in
  [op], ad::ads
