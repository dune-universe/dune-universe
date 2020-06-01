open! Base
open! Stdio
open! Expect_test_helpers_base

let%expect_test "multiple calls to [print_s] create multiple lines" =
  print_s [%message "hello"];
  print_s [%message "there"];
  [%expect {|
    hello
    there
  |}]
;;

let%expect_test "[print_s ~hide_positions:true]" =
  print_s ~hide_positions:true [%message [%here] [%here]];
  [%expect
    {|
      (lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL
       lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[print_string ~hide_positions:true]" =
  print_string ~hide_positions:true (Sexp.to_string_hum [%message [%here] [%here]]);
  [%expect
    {|
      (lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL
       lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[print_endline ~hide_positions:true]" =
  print_endline ~hide_positions:true (Sexp.to_string_hum [%message [%here] [%here]]);
  [%expect
    {|
      (lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL
       lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[~hide_positions:true] for line number from [%of_sexp]" =
  show_raise ~hide_positions:true (fun () -> [%of_sexp: int * int] (List []));
  [%expect
    {|
    (raised (
      Of_sexp_error
      "test_expect_test_helpers_base.ml line LINE: (int * int)_of_sexp: tuple of size 2 expected"
      (invalid_sexp ()))) |}]
;;

let%expect_test "[sexp_style]" =
  let sexp =
    List.init 6 ~f:(fun x -> List.init x ~f:(fun y -> List.init y ~f:(fun z -> x, y, z)))
    |> [%sexp_of: (int * int * int) list list list]
  in
  let test style =
    Ref.set_temporarily sexp_style style ~f:(fun () ->
      print_s sexp;
      require
        [%here]
        (String.is_suffix (sexp_to_string sexp) ~suffix:"\n")
        ~if_false_then_print_s:(lazy [%message "no endline"]))
  in
  test To_string_mach;
  [%expect
    {|
      (()(())(()((2 1 0)))(()((3 1 0))((3 2 0)(3 2 1)))(()((4 1 0))((4 2 0)(4 2 1))((4 3 0)(4 3 1)(4 3 2)))(()((5 1 0))((5 2 0)(5 2 1))((5 3 0)(5 3 1)(5 3 2))((5 4 0)(5 4 1)(5 4 2)(5 4 3)))) |}];
  test To_string_hum;
  [%expect
    {|
    (() (()) (() ((2 1 0))) (() ((3 1 0)) ((3 2 0) (3 2 1)))
     (() ((4 1 0)) ((4 2 0) (4 2 1)) ((4 3 0) (4 3 1) (4 3 2)))
     (() ((5 1 0)) ((5 2 0) (5 2 1)) ((5 3 0) (5 3 1) (5 3 2))
      ((5 4 0) (5 4 1) (5 4 2) (5 4 3)))) |}];
  test Sexp_style.simple_pretty;
  [%expect
    {|
    (()
     (())
     (() ((2 1 0)))
     (() ((3 1 0)) ((3 2 0) (3 2 1)))
     (() ((4 1 0)) ((4 2 0) (4 2 1)) ((4 3 0) (4 3 1) (4 3 2)))
     (()
      ((5 1 0))
      ((5 2 0) (5 2 1))
      ((5 3 0) (5 3 1) (5 3 2))
      ((5 4 0) (5 4 1) (5 4 2) (5 4 3)))) |}];
  test Sexp_style.default_pretty;
  [%expect
    {|
    (()
     (())
     (() ((2 1 0)))
     (()
      ((3 1 0))
      ((3 2 0)
       (3 2 1)))
     (()
      ((4 1 0))
      ((4 2 0)
       (4 2 1))
      ((4 3 0)
       (4 3 1)
       (4 3 2)))
     (()
      ((5 1 0))
      ((5 2 0)
       (5 2 1))
      ((5 3 0)
       (5 3 1)
       (5 3 2))
      ((5 4 0)
       (5 4 1)
       (5 4 2)
       (5 4 3)))) |}]
;;

let%expect_test "[show_raise], no exception" =
  show_raise ~hide_positions:true (fun () -> ());
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "[show_raise], raises hiding positions" =
  show_raise ~hide_positions:true (fun () -> raise_s [%message [%here]]);
  [%expect
    {|
    (raised
     lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[show_raise] with a deep stack" =
  let rec loop n = if n = 0 then failwith "raising" else 1 + loop (n - 1) in
  show_raise (fun () -> loop 13);
  [%expect {|
    (raised (Failure raising)) |}]
;;

let%expect_test "[show_raise] ignores return value" =
  show_raise (fun () -> 13);
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "[require] true prints nothing" =
  require [%here] true;
  [%expect {||}]
;;

let%expect_test "[cr]" =
  print_cr [%here] [%message "some message"] ~cr:Comment;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "some message" |}]
;;

let%expect_test "[require] false respects [~cr] and default [~hide_positions]" =
  require [%here] false ~cr:Comment ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL |}]
;;

let%expect_test "[require false] on non-comment [~cr] values includes instructions" =
  require [%here] false ~cr:CR_someday ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect
    {|
    lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL |}]
;;

let%expect_test "[require_equal] success" =
  require_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect {||}]
;;

let%expect_test "[require_equal] failure" =
  require_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are not equal" 1 2) |}]
;;

let%expect_test "[require_equal] failure with [~message]" =
  require_equal [%here] (module Int) ~cr:Comment 1 2 ~message:"The sky is falling!";
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("The sky is falling!" 1 2) |}]
;;

let%expect_test "[require_equal] failure with [~if_false_then_print_s]" =
  require_equal
    [%here]
    (module Int)
    ~cr:Comment
    1
    2
    ~if_false_then_print_s:(lazy [%message "The sky is falling!"]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are not equal" 1 2 "The sky is falling!") |}]
;;

let%expect_test "[require_compare_equal] success" =
  require_compare_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect {||}]
;;

let%expect_test "[require_compare_equal] failure" =
  require_compare_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are not equal" 1 2) |}]
;;

let%expect_test "[require_compare_equal] failure with [~message]" =
  require_compare_equal
    [%here]
    (module Int)
    ~cr:Comment
    1
    2
    ~message:"The sky is falling!";
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("The sky is falling!" 1 2) |}]
;;

let%expect_test "[require_does_not_raise], no exception" =
  require_does_not_raise [%here] ~hide_positions:true (fun () -> ());
  [%expect {| |}]
;;

let%expect_test "[require_does_not_raise], raises hiding positions" =
  require_does_not_raise [%here] ~cr:Comment (fun () -> raise_s [%message [%here]]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised"
     lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[require_does_not_raise] with a deep stack" =
  let rec loop n = if n = 0 then failwith "raising" else 1 + loop (n - 1) in
  require_does_not_raise [%here] ~cr:Comment (fun () -> ignore (loop 13 : int));
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised" (Failure raising)) |}]
;;

let%expect_test "[require_does_raise] failure" =
  require_does_raise [%here] ~cr:Comment (fun () -> ());
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "did not raise" |}]
;;

let%expect_test "[require_does_raise] success" =
  require_does_raise [%here] (fun () -> raise_s [%message "Boom!"]);
  [%expect {|
    Boom! |}]
;;

let%expect_test "[require_does_raise ~hide_positions:true] success" =
  require_does_raise [%here] ~hide_positions:true (fun () -> raise_s [%message [%here]]);
  [%expect
    {|
    lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL |}]
;;

let%expect_test "[replace]" =
  "/tmp/dir.tmp.123456/file.txt copied from /jenga/root/app/foo/file.txt"
  |> replace ~pattern:"/tmp/dir.tmp.123456" ~with_:"$TMP"
  |> replace ~pattern:"/jenga/root" ~with_:"$ROOT"
  |> print_endline;
  [%expect {| $TMP/file.txt copied from $ROOT/app/foo/file.txt |}]
;;

let%expect_test "[replace_s]" =
  [%sexp
    "copied file"
  , { dst = "/tmp/dir.tmp.123456/file.txt"; src = "/jenga/root/app/foo/file.txt" }]
  |> replace_s ~pattern:"/tmp/dir.tmp.123456" ~with_:"$TMP"
  |> replace_s ~pattern:"/jenga/root" ~with_:"$ROOT"
  |> print_s;
  [%expect
    {|
    ("copied file" (
      (dst $TMP/file.txt)
      (src $ROOT/app/foo/file.txt))) |}]
;;

let%expect_test "hide_temp_files_in_string" =
  "/usr/local/home/non-user.tmp.abcXYZ/file.tmp.r2c3p0.gz"
  |> hide_temp_files_in_string
  |> print_endline;
  [%expect {| /usr/local/home/non-user.tmp.RANDOM/file.tmp.RANDOM.gz |}]
;;

let%expect_test "[require_sets_are_equal] success" =
  require_sets_are_equal
    [%here]
    (module Int)
    (Set.empty (module Int))
    (Set.empty (module Int));
  [%expect {| |}];
  require_sets_are_equal
    [%here]
    (module Int)
    (Set.of_list (module Int) [ 1; 2; 3 ])
    (Set.of_list (module Int) [ 3; 2; 1 ]);
  [%expect {| |}]
;;

let%expect_test "[require_sets_are_equal] failure" =
  require_sets_are_equal
    [%here]
    (module Int)
    ~cr:Comment
    (Set.of_list (module Int) [ 1; 2 ])
    (Set.of_list (module Int) [ 2; 3 ]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal"
      ("in first but not in second" (1))
      ("in second but not in first" (3))) |}]
;;

let%expect_test "[require_sets_are_equal] failure with extras only in first" =
  require_sets_are_equal
    [%here]
    (module Int)
    ~cr:Comment
    (Set.of_list (module Int) [ 1; 2 ])
    (Set.of_list (module Int) [ 2 ]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal" ("in first but not in second" (1))) |}]
;;

let%expect_test "[require_sets_are_equal] failure with extras only in second" =
  require_sets_are_equal
    [%here]
    (module Int)
    ~cr:Comment
    (Set.of_list (module Int) [ 2 ])
    (Set.of_list (module Int) [ 2; 3 ]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal" ("in second but not in first" (3))) |}]
;;

let%expect_test "[require_sets_are_equal] failure with names" =
  require_sets_are_equal
    [%here]
    (module Int)
    (Set.of_list (module Int) [ 1; 2 ])
    (Set.of_list (module Int) [ 2; 3 ])
    ~cr:Comment
    ~names:("expected", "actual");
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal"
      ("in expected but not in actual" (1))
      ("in actual but not in expected" (3))) |}]
;;

let%expect_test "[on_print_cr]" =
  let cr = CR.Comment in
  let hide_positions = true in
  let run () =
    print_cr [%here] ~cr ~hide_positions [%message "unconditional message"];
    require
      [%here]
      ~cr
      ~hide_positions
      false
      ~if_false_then_print_s:(lazy [%message "conditional message"]);
    require
      [%here]
      ~cr
      ~hide_positions
      true
      ~if_false_then_print_s:(lazy [%message "elided message"])
  in
  let default = !on_print_cr in
  run ();
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "unconditional message"
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "conditional message" |}];
  on_print_cr := ignore;
  run ();
  [%expect {||}];
  (on_print_cr := fun string -> print_endline (String.uppercase string));
  run ();
  [%expect
    {|
    (* REQUIRE-FAILED: LIB/EXPECT_TEST_HELPERS/BASE/TEST/TEST_EXPECT_TEST_HELPERS_BASE.ML:LINE:COL. *)
    "UNCONDITIONAL MESSAGE"
    (* REQUIRE-FAILED: LIB/EXPECT_TEST_HELPERS/BASE/TEST/TEST_EXPECT_TEST_HELPERS_BASE.ML:LINE:COL. *)
    "CONDITIONAL MESSAGE" |}];
  on_print_cr := default;
  run ();
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "unconditional message"
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "conditional message" |}]
;;

let%expect_test "[quickcheck] success" =
  quickcheck
    [%here]
    Base_quickcheck.quickcheck_generator_int
    ~sexp_of:Int.sexp_of_t
    ~f:ignore;
  [%expect {||}]
;;

(* Quickcheck pseudo-random generation is different on 32-bit and 64-bit, so we only run
   Quickcheck failure tests on 64-bit builds. *)

let%expect_test ("[quickcheck] failure"[@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck
    [%here]
    ~cr
    Base_quickcheck.quickcheck_generator_int
    ~sexp_of:Int.sexp_of_t
    ~f:(fun int ->
      require [%here] ~cr (int > 100) ~if_false_then_print_s:(lazy [%message "BAD"]));
  [%expect
    {|
    ("quickcheck: test failed" (input -15508265059))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    BAD |}]
;;

let%expect_test ("[quickcheck] failure with multiple CRs"[@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck
    [%here]
    ~cr
    Base_quickcheck.quickcheck_generator_int
    ~sexp_of:Int.sexp_of_t
    ~f:(fun _ ->
      print_cr [%here] ~cr [%message "first"];
      require [%here] ~cr false ~if_false_then_print_s:(lazy [%message "second"]));
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    first
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    second |}]
;;

let%expect_test ("[quickcheck] raised exception"[@tags "64-bits-only"]) =
  let cr = CR.Comment in
  require_does_not_raise [%here] (fun () ->
    quickcheck
      [%here]
      ~cr
      Base_quickcheck.quickcheck_generator_int
      ~sexp_of:Int.sexp_of_t
      ~f:(fun int -> if int > 100 then raise_s [%message "BAD"]));
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised" BAD) |}]
;;

let%expect_test ("[quickcheck] failure with shrinker"[@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck
    [%here]
    ~cr
    (Base_quickcheck.Generator.return 10)
    ~sexp_of:[%sexp_of: int]
    ~shrinker:(Base_quickcheck.Shrinker.create (fun int -> Sequence.singleton (int - 1)))
    ~f:(fun int ->
      require
        [%here]
        ~cr
        (int <= 0)
        ~if_false_then_print_s:(lazy [%message "positive" ~_:(int : int)]));
  [%expect
    {|
    ("quickcheck: test failed" (input 1))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    (positive 1) |}]
;;
