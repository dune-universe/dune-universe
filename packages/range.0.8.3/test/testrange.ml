(* SPDX-License-Identifier:  GPL-3.0-or-later *)
open OUnit2
open Base

let natural_modified_consistency_tests =
  [
    ( "Issue #1 : A modified range can be equal to a natural one"
    >:: fun _ctxt ->
      let open Range in
      let data_a = from 3 6 in
      let data_b = from 2 5 |> map ~f:Int.succ in
      assert_bool "equality failed" (equal data_a data_b) );
    ( "Issue #2 : length of a range must consider modified use cases"
    >:: fun ctxt ->
      let open Range in
      let data = from 6 10 |> filter ~f:(( <> ) 8) in
      let expected = 4 in
      assert_equal ~cmp:Int.equal ~printer:Int.to_string ~ctxt expected
        (length data) );
  ]

let discovered_bugs =
  [
    ( "Issue #5 : split distribution equity" >:: fun ctxt ->
      let open Range in
      let three = Number.gtz_from_int_exn 3 in
      let result =
        from 2 12 |> split three three |> List.map ~f:to_string
        |> String.concat ~sep:"/"
      in
      let expected = "Nat:10:12/Nat:6:9/Nat:2:5" in
      assert_equal ~cmp:String.equal ~printer:Fn.id ~ctxt expected result );
  ]

let non_reg_tests =
  [ natural_modified_consistency_tests; discovered_bugs ] |> List.concat

let () = run_test_tt_main ("Range non regression bug testing" >::: non_reg_tests)
