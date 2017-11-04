open Sexplib.Std

type t = int [@@deriving_inline sexp]
let _ = fun (_ : t)  -> ()
let t_of_sexp : Sexplib.Sexp.t -> t =
  let _tp_loc = "test.ml.t"  in fun t  -> int_of_sexp t
let _ = t_of_sexp
let sexp_of_t : t -> Sexplib.Sexp.t = fun v  -> sexp_of_int v
let _ = sexp_of_t
[@@@end]

let%expect_test _ =
  Printf.printf "%d\n" [%omp_test];
  [%expect {| 42 |}]
