(* Test suite which runs under nodejs as Javascript. *)

let suite =
  let open Webtest.Suite in
  "nodejs_suite" >::: [
    Test_assert.suite;
    Test_async.suite;
    Test_js.suite;
    Test_sync.suite;
  ]

let () = Webtest_js.Runner.run suite
