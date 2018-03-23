open! Core_kernel
open! Import

let%expect_test
  "[print_and_check_stable_type] shows [Shape.Digest] even for empty examples"
  =
  print_and_check_stable_type [%here] (module Int) [];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac) |}];
;;

let%expect_test "[print_and_check_stable_type]" =
  print_and_check_stable_type [%here] (module Int) [ 0; 21; Int.max_value_30_bits ];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   0)
     (bin_io "\000"))
    ((sexp   21)
     (bin_io "\021"))
    ((sexp   1073741823)
     (bin_io "\253\255\255\255?")) |}];
;;

let%expect_test "[print_and_check_stable_type] with broken round-trip" =
  let module Broken = struct
    type t = int [@@deriving compare]
    let to_serializeable t = Int.to_string_hum t
    let of_serializeable _ = 42
    include Sexpable.Of_sexpable (String) (struct
        type t = int
        let to_sexpable = to_serializeable
        let of_sexpable = of_serializeable
      end)
    include Binable.Of_binable (String) (struct
        type t = int
        let to_binable = to_serializeable
        let of_binable = of_serializeable
      end)
  end in
  print_and_check_stable_type [%here] ~cr:Comment (module Broken)
    [ 42; 23 ];
  [%expect {|
    (bin_shape_digest d9a8da25d5656b016fb4dbdc2e4197fb)
    ((sexp   42)
     (bin_io "\00242"))
    ((sexp   23)
     (bin_io "\00223"))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       23)
      (sexp           23)
      (sexp_roundtrip 42))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("bin-io serialization failed to round-trip"
     (original         23)
     (bin_io           "\00223")
     (bin_io_roundtrip 42)) |}];
;;

let%expect_test "[print_and_check_stable_type] with exceeded max-length" =
  print_and_check_stable_type [%here] ~cr:Comment (module Int)
    [ 0; Int.max_value_30_bits ]
    ~max_binable_length:1;
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   0)
     (bin_io "\000"))
    ((sexp   1073741823)
     (bin_io "\253\255\255\255?"))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("bin-io serialization exceeds max binable length"
     (original           1073741823)
     (bin_io             "\253\255\255\255?")
     (bin_io_length      5)
     (max_binable_length 1)) |}];
;;

let%expect_test "[print_and_check_stable_type] with conversion that raises" =
  let module Broken = struct
    type t = int [@@deriving sexp, bin_io]
    let compare x y = raise_s [%message "compare" (x : int) (y : int)]
  end in
  print_and_check_stable_type [%here] ~cr:Comment (module Broken)
    [ 1; 2; 3 ];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   1)
     (bin_io "\001"))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("unexpectedly raised" (
      compare
      (x 1)
      (y 1))) |}];
;;

let%expect_test "multiple calls to [print_s] create multiple lines" =
  print_s [%message "hello"];
  print_s [%message "there"];
  [%expect {|
    hello
    there
  |}];
;;

let%expect_test "[print_s ~hide_positions:true]" =
  print_s ~hide_positions:true [%message [%here] [%here]];
  [%expect {|
      (lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL
       lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL) |}];
;;

let%expect_test "[~hide_positions:true] for line number from [%of_sexp]" =
  show_raise ~hide_positions:true (fun () -> [%of_sexp: int * int] (List []));
  [%expect {|
    (raised (
      Sexplib.Conv.Of_sexp_error
      (Failure
       "test_helpers.ml line LINE: (int * int)_of_sexp: tuple of size 2 expected")
      ())) |}];
;;

let%expect_test "[print_s] bug, apparently" =
  "(\"sets are not equal\"(first (1 2))(second (2))(\"in first but not in second\"(1)))"
  |> Sexp.of_string
  |> print_s;
  [%expect {|
    ("sets are not equal"
      (first (1 2))
      (second                     (2))
      ("in first but not in second"(
      1))) |}];
;;

let%expect_test "[show_raise], no exception" =
  show_raise ~hide_positions:true (fun () -> ());
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "[show_raise], raises hiding positions" =
  show_raise ~hide_positions:true (fun () -> raise_s [%message [%here]]);
  [%expect {|
    (raised lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL) |}]
;;

let%expect_test "[show_raise] with a deep stack" =
  let rec loop n =
    if n = 0
    then failwith "raising"
    else 1 + loop (n - 1)
  in
  show_raise (fun () -> loop 13);
  [%expect {|
    (raised (Failure raising)) |}];
;;

let%expect_test "[show_raise] ignores return value" =
  show_raise (fun () -> 13);
  [%expect {|
    "did not raise" |}];
;;

let%expect_test "[require] true prints nothing" =
  require [%here] true;
  [%expect {||}];
;;

let%expect_test "[cr]" =
  print_cr [%here] [%message "some message"] ~cr:Comment;
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    "some message" |}];
;;

let%expect_test "[require] false respects [~cr] and default [~hide_positions]" =
  require [%here] false ~cr:Comment
    ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL |}];
;;

let%expect_test "[require false] on non-comment [~cr] values includes instructions" =
  require [%here] false ~cr:CR_someday
    ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect {|
    lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL |}];
;;

let%expect_test "[require_equal] success" =
  require_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect {||}];
;;

let%expect_test "[require_equal] failure" =
  require_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("values are not equal" 1 2) |}];
;;

let%expect_test "[require_equal] failure with [~message]" =
  require_equal [%here] (module Int) ~cr:Comment 1 2
    ~message:"The sky is falling!";
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("The sky is falling!" 1 2) |}];
;;

let%expect_test "[require_equal] failure with [~if_false_then_print_s]" =
  require_equal [%here] (module Int) ~cr:Comment 1 2
    ~if_false_then_print_s:(lazy [%message "The sky is falling!"]);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("values are not equal" 1 2 "The sky is falling!") |}];
;;

let%expect_test "[require_compare_equal] success" =
  require_compare_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect {||}];
;;

let%expect_test "[require_compare_equal] failure" =
  require_compare_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("values are not equal" 1 2) |}];
;;

let%expect_test "[require_compare_equal] failure with [~message]" =
  require_compare_equal [%here] (module Int) ~cr:Comment 1 2
    ~message:"The sky is falling!";
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("The sky is falling!" 1 2) |}];
;;

let%expect_test "[require_sets_are_equal] success" =
  require_sets_are_equal [%here] (module Int.Set) Int.Set.empty Int.Set.empty;
  [%expect {| |}];
  require_sets_are_equal [%here] (module Int.Set)
    (Int.Set.of_list [ 1; 2; 3 ])
    (Int.Set.of_list [ 3; 2; 1 ]);
  [%expect {| |}];
;;

let%expect_test "[require_sets_are_equal] failure" =
  require_sets_are_equal [%here] (module Int.Set)
    ~cr:Comment
    (Int.Set.of_list [ 1; 2 ])
    (Int.Set.of_list [ 2; 3 ]);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("sets are not equal"
      ("in first but not in second" (1))
      ("in second but not in first" (3))) |}];
;;

let%expect_test "[require_sets_are_equal] failure with extras only in first" =
  require_sets_are_equal [%here] (module Int.Set)
    ~cr:Comment
    (Int.Set.of_list [ 1; 2 ])
    (Int.Set.of_list [ 2 ]);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("sets are not equal" ("in first but not in second" (1))) |}];
;;

let%expect_test "[require_sets_are_equal] failure with extras only in second" =
  require_sets_are_equal [%here] (module Int.Set)
    ~cr:Comment
    (Int.Set.of_list [ 2 ])
    (Int.Set.of_list [ 2; 3 ]);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("sets are not equal" ("in second but not in first" (3))) |}];
;;

let%expect_test "[require_sets_are_equal] failure with names" =
  require_sets_are_equal [%here] (module Int.Set)
    (Int.Set.of_list [ 1; 2 ])
    (Int.Set.of_list [ 2; 3 ])
    ~cr:Comment
    ~names:("expected", "actual");
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("sets are not equal"
      ("in expected but not in actual" (1))
      ("in actual but not in expected" (3))) |}];
;;

let%expect_test "[require_does_not_raise], no exception" =
  require_does_not_raise [%here] ~hide_positions:true (fun () -> ());
  [%expect {| |}]
;;

let%expect_test "[require_does_not_raise], raises hiding positions" =
  require_does_not_raise [%here] ~cr:Comment (fun () ->
    raise_s [%message [%here]]);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("unexpectedly raised"
     lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL) |}]
;;

let%expect_test "[require_does_not_raise] with a deep stack" =
  let rec loop n =
    if n = 0
    then failwith "raising"
    else 1 + loop (n - 1)
  in
  require_does_not_raise [%here] ~cr:Comment (fun () ->
    ignore (loop 13 : int));
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("unexpectedly raised" (Failure raising)) |}];
;;

let%expect_test "[require_does_raise] failure" =
  require_does_raise [%here] ~cr:Comment (fun () -> ());
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    "did not raise" |}];
;;

let%expect_test "[require_does_raise] success" =
  require_does_raise [%here] (fun () -> raise_s [%message "Boom!"]);
  [%expect {|
    Boom! |}];
;;

let%expect_test "[require_does_raise ~hide_positions:true] success" =
  require_does_raise [%here] ~hide_positions:true (fun () -> raise_s [%message [%here]]);
  [%expect {|
    lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL |}];
;;

let%expect_test "[require_no_allocation] ignores non-allocating functions" =
  require_no_allocation ~cr:Comment [%here] (fun () -> ());
  [%expect {| |}];
;;

let%expect_test "[require_no_allocation] shows breach and expected, but does not show allocation" =
  ignore (require_no_allocation ~cr:Comment [%here]
            (fun () -> List.map [1; 2; 3] ~f:(fun i -> i + 1))
          : int list);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("allocation exceeded limit" (allocation_limit (Minor_words 0))) |}];
;;

let%expect_test "[require_allocation_does_not_exceed] shows breach but not allocation" =
  ignore (require_allocation_does_not_exceed
            ~cr:Comment
            (Minor_words 1)
            [%here]
            (fun () -> List.map [1; 2; 3] ~f:(fun i -> i + 1))
          : int list);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("allocation exceeded limit" (allocation_limit (Minor_words 1))) |}];
;;

let%expect_test "[print_and_check_container_sexps] success" =
  print_and_check_container_sexps [%here] (module Int) [1; 10; 100];
  [%expect {|
    (Set (1 10 100))
    (Map (
      (1   0)
      (10  1)
      (100 2)))
    (Hash_set (1 10 100))
    (Table (
      (1   0)
      (10  1)
      (100 2))) |}];
;;

let%expect_test "[print_and_check_container_sexps] failure" =
  print_and_check_container_sexps ~cr:Comment [%here]
    (module struct
      include Int
      let sexp_of_t = Int.Hex.sexp_of_t
    end)
    [1; 10; 100];
  [%expect {|
    (Set (1 10 100))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("set sexp does not match sorted list sexp"
      (set_sexp         (1   10  100))
      (sorted_list_sexp (0x1 0xa 0x64)))
    (Map (
      (1   0)
      (10  1)
      (100 2)))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("map sexp does not match sorted alist sexp"
     (map_sexp (
       (1   0)
       (10  1)
       (100 2)))
     (sorted_alist_sexp (
       (0x1  0)
       (0xa  1)
       (0x64 2))))
    (Hash_set (1 10 100))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("hash_set sexp does not match sorted list sexp"
     (hash_set_sexp    (1   10  100))
     (sorted_list_sexp (0x1 0xa 0x64)))
    (Table (
      (1   0)
      (10  1)
      (100 2)))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("table sexp does not match sorted alist sexp"
     (table_sexp (
       (1   0)
       (10  1)
       (100 2)))
     (sorted_alist_sexp (
       (0x1  0)
       (0xa  1)
       (0x64 2)))) |}];
;;

let%expect_test "[on_print_cr]" =
  let cr = CR.Comment in
  let hide_positions = true in
  let run () =
    print_cr [%here] ~cr ~hide_positions [%message "unconditional message"];
    require [%here] ~cr ~hide_positions false
      ~if_false_then_print_s:(lazy [%message "conditional message"]);
    require [%here] ~cr ~hide_positions true
      ~if_false_then_print_s:(lazy [%message "elided message"]);
  in
  let default = !on_print_cr in
  run ();
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    "unconditional message"
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    "conditional message" |}];
  on_print_cr := ignore;
  run ();
  [%expect {||}];
  on_print_cr := (fun string -> print_endline (String.uppercase string));
  run ();
  [%expect {|
    (* REQUIRE-FAILED: LIB/EXPECT_TEST_HELPERS_KERNEL/TEST/TEST_HELPERS.ML:LINE:COL. *)
    "UNCONDITIONAL MESSAGE"
    (* REQUIRE-FAILED: LIB/EXPECT_TEST_HELPERS_KERNEL/TEST/TEST_HELPERS.ML:LINE:COL. *)
    "CONDITIONAL MESSAGE" |}];
  on_print_cr := default;
  run ();
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    "unconditional message"
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    "conditional message" |}];
;;

let%expect_test "[quickcheck] success" =
  quickcheck [%here] Int.gen ~sexp_of:Int.sexp_of_t ~f:ignore;
  [%expect {||}];
;;

(* Quickcheck pseudo-random generation is different on 32-bit and 64-bit, so we only run
   Quickcheck failure tests on 64-bit builds. *)

let%expect_test "[quickcheck] failure" [@tags "64-bits-only"] =
  let cr = CR.Comment in
  quickcheck [%here] ~cr Int.gen ~sexp_of:Int.sexp_of_t ~f:(fun int ->
    require [%here] ~cr (int > 100) ~if_false_then_print_s:(lazy [%message "BAD"]));
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    BAD
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("random input"
      (value -15508265059)
      (error "printed 1 CRs for Quickcheck-generated input")) |}]
;;

let%expect_test "[quickcheck] failure with multiple CRs" [@tags "64-bits-only"] =
  let cr = CR.Comment in
  quickcheck [%here] ~cr Int.gen ~sexp_of:Int.sexp_of_t ~f:(fun _ ->
    print_cr [%here] ~cr [%message "first"];
    require [%here] ~cr false ~if_false_then_print_s:(lazy [%message "second"]));
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    first
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    second
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("random input"
      (value 76753)
      (error "printed 2 CRs for Quickcheck-generated input")) |}]
;;

let%expect_test "[quickcheck] raised exception" [@tags "64-bits-only"] =
  let cr = CR.Comment in
  show_raise (fun () ->
    quickcheck [%here] ~cr Int.gen ~sexp_of:Int.sexp_of_t ~f:(fun int ->
      if int > 100 then raise_s [%message "BAD"]));
  [%expect {|
    (raised BAD) |}]
;;

let%expect_test "[quickcheck] failure with shrinker" [@tags "64-bits-only"] =
  let cr = CR.Comment in
  quickcheck [%here]
    ~cr
    (Quickcheck.Generator.return 10)
    ~sexp_of:[%sexp_of: int]
    ~shrinker:(Quickcheck.Shrinker.create (fun int -> Sequence.singleton (int - 1)))
    ~f:(fun int ->
      require [%here] ~cr (int <= 0)
        ~if_false_then_print_s:(lazy [%message "positive" ~_:(int : int)]));
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 10)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 9)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 8)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 7)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 6)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 5)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 4)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 3)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 2)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (positive 1)
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("shrunk random input"
      (shrunk_value 1)
      (shrunk_error "printed 1 CRs for Quickcheck-generated input")
      (original_value 10)
      (original_error "printed 1 CRs for Quickcheck-generated input")) |}]
;;
