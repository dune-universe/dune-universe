open! Core
open! Async
open! Import

let%expect_test "[require]" =
  require [%here] true;
  let%bind () = [%expect {|
    |}] in
  require [%here] false ~cr:CR_someday;
  let%bind () =
    [%expect
      {|
    |}]
  in
  require
    [%here]
    false
    ~cr:Comment
    ~if_false_then_print_s:(lazy [%sexp "additional-info"]);
  let%bind () =
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/async/test/test_require.ml:LINE:COL. *)
    additional-info |}]
  in
  return ()
;;
