open! Core
open! Async
open! Import

let%expect_test "without an [%expect]" =
  Printexc.record_backtrace false;
  assert false
[@@expect.uncaught_exn
  {|
  (monitor.ml.Error "Assert_failure test_uncaught_exn.ml:7:2"
    ("<backtrace elided in test>" "Caught by monitor block_on_async")) |}]
;;

let%expect_test "with an [%expect]" =
  Printexc.record_backtrace false;
  ignore (assert false);
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (monitor.ml.Error "Assert_failure test_uncaught_exn.ml:16:9"
    ("<backtrace elided in test>" "Caught by monitor block_on_async")) |}]
;;
