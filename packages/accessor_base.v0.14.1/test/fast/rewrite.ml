open! Core_kernel
open! Import

type t =
  | S
  | K
  | I
  | Apply of t * t
[@@deriving accessors, sexp_of]

let children = [%accessor apply @> Accessor.Tuple2.each]

let evaluate t =
  Accessor.rewrite children t ~f:(function
    | Apply (Apply (Apply (S, x), y), z) -> Some (Apply (Apply (x, z), Apply (y, z)))
    | Apply (Apply (K, x), _) -> Some x
    | Apply (I, x) -> Some x
    | _ -> None)
;;

let%expect_test "rewrite" =
  let program = Apply (Apply (Apply (S, Apply (K, K)), Apply (Apply (S, I), I)), I) in
  print_s [%sexp (evaluate program : t)];
  [%expect {| (Apply K I) |}]
;;
