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
  print_and_check_stable_type [%here] (module Int) [ 0; 21; Int.max_value ];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   0)
     (bin_io "\000"))
    ((sexp   21)
     (bin_io "\021"))
    ((sexp   4611686018427387903)
     (bin_io "\252\255\255\255\255\255\255\255?")) |}];
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
  print_and_check_stable_type [%here] ~cr:Comment ~hide_positions:true (module Broken)
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
  print_and_check_stable_type [%here] ~cr:Comment ~hide_positions:true (module Int)
    [ 0; Int.max_value ]
    ~max_binable_length:1;
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   0)
     (bin_io "\000"))
    ((sexp   4611686018427387903)
     (bin_io "\252\255\255\255\255\255\255\255?"))
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("bin-io serialization exceeds max binable length"
     (original           4611686018427387903)
     (bin_io             "\252\255\255\255\255\255\255\255?")
     (bin_io_length      9)
     (max_binable_length 1)) |}];
;;

let%expect_test "[print_and_check_stable_type] with conversion that raises" =
  let module Broken = struct
    type t = int [@@deriving sexp, bin_io]
    let compare x y = raise_s [%message "compare" (x : int) (y : int)]
  end in
  print_and_check_stable_type [%here] ~cr:Comment ~hide_positions:true (module Broken)
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

let%expect_test "[print_bin_ios] shows [Shape.Digest] even for empty examples" =
  print_bin_ios (module Int) [];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac) |}];
;;

let%expect_test "[print_bin_ios]" =
  print_bin_ios (module Int) [ 0; 21; Int.max_value ];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    "\000"
    "\021"
    "\252\255\255\255\255\255\255\255?" |}];
;;

let%expect_test "[print_bin_ios_with_max]" =
  print_bin_ios_with_max [%here] ~cr:Comment ~hide_positions:true
    (module struct
      include Int
      let max_binable_length = 1
    end)
    [ 0; 21; Int.max_value - 1; Int.max_value ];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    "\000"
    "\021"
    "\252\254\255\255\255\255\255\255?"
    "\252\255\255\255\255\255\255\255?"
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("Maximum binable length exceeded"
      (maximum 1)
      (failures (
        ((value         4611686018427387903)
         (length        9)
         (serialization "\252\255\255\255\255\255\255\255?"))
        ((value         4611686018427387902)
         (length        9)
         (serialization "\252\254\255\255\255\255\255\255?"))))) |}];
;;

let%expect_test "multiple calls to [print_s] create multiple lines" =
  print_s [%message "hello"];
  print_s [%message "there"];
  [%expect {|
    hello
    there
  |}];
;;

(* Get a sexp for the second frame of a backtrace. This frame is stable when
   switching to and from flambda. *)
let sexp_of_second_frame trace =
  match [%sexp (trace : Backtrace.t)] with
  | List (_ :: frame :: _) -> frame
  | _ -> assert false

let%expect_test "[print_s ~hide_positions:true]" =
  print_s ~hide_positions:true [%message [%here] [%here]];
  [%expect {|
    (lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL
     lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL) |}];
  let rec f n =
    if n = 0
    then (
      print_s ~hide_positions:true
        (sexp_of_second_frame (Backtrace.get () ~at_most_num_frames:3));
      0)
    else 1 + f (n - 1)
  in
  ignore (f 10);
  [%expect {|
    "Called from file \"test_helpers.ml\", line LINE, characters C1-C2" |}];
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

let%expect_test "[show_allocation] shows allocation" =
  ignore (show_allocation (fun () -> List.map [1; 2; 3] ~f:(fun i -> i + 1)) : int list);
  [%expect {|
    (allocated
      (minor_words 9)
      (major_words 0)) |}];
;;

let%expect_test "[require] true prints nothing" =
  require [%here] true;
  [%expect {||}];
;;

let%expect_test "[require] false respects [~cr] and [~hide_positions]" =
  require [%here] false ~cr:Comment ~hide_positions:true
    ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL |}];
;;

let%expect_test "[require false] on non-comment [~cr] values includes instructions" =
  require [%here] false ~cr:CR_someday ~hide_positions:true
    ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect {|
    lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL |}];
;;

let%expect_test "[require_does_not_raise], no exception" =
  require_does_not_raise [%here] ~hide_positions:true (fun () -> ());
  [%expect {| |}]
;;

let%expect_test "[require_does_not_raise], raises hiding positions" =
  require_does_not_raise [%here] ~cr:Comment ~hide_positions:true (fun () ->
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
  require_does_not_raise [%here] ~cr:Comment ~hide_positions:true (fun () ->
    ignore (loop 13 : int));
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    ("unexpectedly raised" (Failure raising)) |}];
;;

let%expect_test "[require_no_allocation] ignores non-allocating functions" =
  require_no_allocation ~cr:Comment [%here] (fun () -> ());
  [%expect {| |}];
;;

let%expect_test "[require_no_allocation] shows non-zero allocation" =
  ignore (require_no_allocation ~hide_positions:true ~cr:Comment [%here]
            (fun () -> List.map [1; 2; 3] ~f:(fun i -> i + 1))
          : int list);
  [%expect {|
    (* require-failed: lib/expect_test_helpers_kernel/test/test_helpers.ml:LINE:COL. *)
    (allocated
      (minor_words 9)
      (major_words 0)) |}];
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
  print_and_check_container_sexps ~cr:Comment ~hide_positions:true [%here]
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
