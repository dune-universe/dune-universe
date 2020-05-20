(* Test handling of asynchronous test cases. *)

open Test_utils
open Webtest.Suite

let test_wrapper wrapper = wrapper Async.noop

let test_run_one_ok () =
  assert_equal
    (run_one_sync (fun wrapper -> wrapper Async.noop))
    (Some Pass)

let test_run_one_fail () =
  assert_equal
    (run_one_sync (fun _ -> assert_equal 5 6))
    (Some (Fail "not equal"))

let test_run_one_error () =
  assert_equal
    (run_one_sync (fun _ -> failwith "fail"))
    (Some (Error (Failure "fail")))

let test_run_one_fail_in_callback () =
  assert_equal
    (run_one_sync (fun wrapper -> wrapper (fun () -> assert_equal 5 6)))
    (Some (Fail "not equal"))

let test_run_one_error_in_callback () =
  assert_equal
    (run_one_sync (fun wrapper -> wrapper (fun () -> failwith "fail")))
    (Some (Error (Failure "fail")))

let test_of_sync_ok () =
  let async_test = Async.of_sync Async.noop in
  assert_equal
    (run_one_sync async_test)
    (Some Pass)

let test_of_sync_fail () =
  let async_test = Async.of_sync (fun () -> assert_equal 5 6) in
  assert_equal
    (run_one_sync async_test)
    (Some (Fail "not equal"))

let test_of_sync_error () =
  let async_test = Async.of_sync (fun () -> failwith "fail") in
  assert_equal
    (run_one_sync async_test)
    (Some (Error (Failure "fail")))

let test_bracket_ok () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    assert_equal !state `test_end;
    state := `torn_down
  in
  let async_test = Async.bracket
    setup
    (fun state wrapper ->
      wrapper (fun () ->
        assert_equal !state `test_start;
        state := `test_end))
    teardown
  in
  assert_equal
    (run_one_sync async_test)
    (Some Pass);
  assert_equal !state `torn_down

let test_bracket_fail_in_sync () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    state := `torn_down
  in
  let async_test = Async.bracket
    setup
    (fun _ _ -> assert_equal 5 6)
    teardown
  in
  assert_equal
    (run_one_sync async_test)
    (Some (Fail "not equal"));
  assert_equal !state `torn_down

let test_bracket_fail_in_async () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    state := `torn_down
  in
  let async_test = Async.bracket
    setup
    (fun _ wrapper -> wrapper (fun () -> assert_equal 5 6))
    teardown
  in
  assert_equal
    (run_one_sync async_test)
    (Some (Fail "not equal"));
  assert_equal !state `torn_down

exception TestException

let test_bracket_error_in_sync () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    state := `torn_down
  in
  let async_test = Async.bracket
    setup
    (fun _ _ -> raise TestException)
    teardown
  in
  assert_equal
    (run_one_sync async_test)
    (Some (Error TestException));
  assert_equal !state `torn_down

let test_bracket_error_in_async () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    state := `torn_down
  in
  let async_test = Async.bracket
    setup
    (fun _ wrapper -> wrapper (fun () -> raise TestException))
    teardown
  in
  assert_equal
    (run_one_sync async_test)
    (Some (Error TestException));
  assert_equal !state `torn_down

let suite =
  "async" >::: [
    "test_wrapper" >:~ test_wrapper;
    "test_run_one_ok" >:: test_run_one_ok;
    "test_run_one_fail" >:: test_run_one_fail;
    "test_run_one_error" >:: test_run_one_error;
    "test_run_one_fail_in_callback" >:: test_run_one_fail_in_callback;
    "test_run_one_error_in_callback" >:: test_run_one_error_in_callback;
    "test_of_sync_ok" >:: test_of_sync_ok;
    "test_of_sync_fail" >:: test_of_sync_fail;
    "test_of_sync_error" >:: test_of_sync_error;
    "test_bracket_ok" >:: test_bracket_ok;
    "test_bracket_fail_in_sync" >::  test_bracket_fail_in_sync;
    "test_bracket_fail_in_async" >::  test_bracket_fail_in_async;
    "test_bracket_error_in_sync" >::  test_bracket_error_in_sync;
    "test_bracket_error_in_async" >::  test_bracket_error_in_async;
  ]
