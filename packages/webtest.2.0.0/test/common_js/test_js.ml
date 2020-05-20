(* Javascript-specific tests. *)

open Js_of_ocaml
open Test_utils
open Webtest.Suite

let test_async_wrapper wrapper =
  let (_:Dom_html.timeout_id_safe) =
    Dom_html.setTimeout (fun () -> wrapper Async.noop) 0.5 in ()

let test_run_one_failure_in_sync_path () =
  assert_equal
    (run_one_sync (fun wrapper ->
      let (_:Dom_html.timeout_id_safe) =
        Dom_html.setTimeout (fun () -> wrapper Async.noop) 0.5 in

      assert_equal 5 6))
    (Some (Fail "not equal"))

let test_run_one_error_in_sync_path () =
  assert_equal
    (run_one_sync (fun wrapper ->
      let (_:Dom_html.timeout_id_safe) =
        Dom_html.setTimeout (fun () -> wrapper Async.noop) 0.5 in

      failwith "fail"))
    (Some (Error (Failure "fail")))

let suite =
  "js" >::: [
    "test_async_wrapper" >:~ test_async_wrapper;
    "test_run_one_failure_in_sync_path" >:: test_run_one_failure_in_sync_path;
    "test_run_one_error_in_sync_path" >:: test_run_one_error_in_sync_path;
  ]
