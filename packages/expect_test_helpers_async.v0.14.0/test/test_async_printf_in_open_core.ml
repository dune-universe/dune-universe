open! Core

(* No [open! Import] because we want to be very explicit about the environment. *)

let%expect_test "" =
  Expect_test_helpers_core.require ~cr:Comment [%here] false;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/async/test/test_async_printf_in_open_core.ml:LINE:COL. *) |}]
;;
