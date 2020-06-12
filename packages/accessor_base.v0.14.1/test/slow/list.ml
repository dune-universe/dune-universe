open! Core_kernel
open! Import

let%expect_test "no stack overflow on List.each and List.eachi" =
  let large_enough_to_trigger_the_stack_overflow_bug_that_used_to_exist = 10_000_000 in
  let xs =
    List.init large_enough_to_trigger_the_stack_overflow_bug_that_used_to_exist ~f:Fn.id
  in
  Expect_test_helpers_core.require_does_not_raise ~cr:CR_soon [%here] (fun () ->
    Accessor.iter Accessor.List.each xs ~f:(fun (_ : int) -> ());
    Accessor.iteri Accessor.List.eachi xs ~f:(fun [ index ] value ->
      [%test_eq: int] index value));
  [%expect {| |}]
;;

let%expect_test "List.eachi works as expected for empty list" =
  Accessor.iteri Accessor.List.eachi [] ~f:(fun [ index ] value ->
    print_s [%message "unexpected" (index : int) (value : int)]);
  [%expect {| |}]
;;
