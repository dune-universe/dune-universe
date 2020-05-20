(* Test suite which runs as OCaml. *)

open Webtest

let suite =
  let open Webtest.Suite in
  "ocaml_suite" >::: [
    Test_assert.suite;
    Test_async.suite;
    Test_sync.suite;
  ]

let () =
  Utils.run suite
    (fun {Utils.log; outcomes} ->
      let {Utils.report; passed} = Utils.summarise outcomes in
      let log = (String.concat "\n" log) in
      print_endline log;
      print_endline report;
      if not passed
      then exit 1)
