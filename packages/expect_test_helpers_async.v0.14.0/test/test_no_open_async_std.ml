open! Core

(* No [open Async] to check that [print_s] works correctly in its absence. *)
open! Import

let%expect_test _ =
  print_s [%message "hello"];
  [%expect {| hello |}]
;;
