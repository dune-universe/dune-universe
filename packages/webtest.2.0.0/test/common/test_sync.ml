(* Test handling of synchronous test cases. *)

open Webtest.Suite

let test_bracket_succeed () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    assert_equal !state `test_end;
    state := `torn_down
  in
  Sync.bracket
    setup
    (fun state ->
      assert_equal !state `test_start;
      state := `test_end)
    teardown ();
  assert_equal !state `torn_down

let test_bracket_fail () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    state := `torn_down
  in
  try
    Sync.bracket
      setup
      (fun state ->
        assert_equal !state `test_start;
        assert_equal 5 6)
      teardown ();
  with TestFailure "not equal" ->
    assert_equal !state `torn_down

exception TestException
let test_bracket_error () =
  let state = ref `uninitialised in
  let setup () = state := `test_start; state in
  let teardown state =
    state := `torn_down
  in
  try
    Sync.bracket
      setup
      (fun state ->
        assert_equal !state `test_start;
        raise TestException)
      teardown ();
  with TestException ->
    assert_equal !state `torn_down

let suite =
  "sync" >::: [
    "test_bracket_succeed" >:: test_bracket_succeed;
    "test_bracket_fail" >:: test_bracket_fail;
    "test_bracket_error" >:: test_bracket_error;
  ]
