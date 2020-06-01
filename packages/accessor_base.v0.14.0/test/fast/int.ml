open! Core_kernel
open! Import

let%expect_test "add" =
  print_s [%sexp (Accessor.map (Accessor.Int.added 5) 1 ~f:(fun x -> x * 2) : int)];
  (* ((1 + 5) * 2) - 5 *)
  [%expect {| 7 |}]
;;

let%expect_test "subtract" =
  print_s [%sexp (Accessor.map (Accessor.Int.subtracted 5) 20 ~f:(fun x -> x * 2) : int)];
  (* ((20 - 5) * 2) + 5 *)
  [%expect {| 35 |}]
;;
