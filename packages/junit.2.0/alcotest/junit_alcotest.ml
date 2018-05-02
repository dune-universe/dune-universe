module A = Alcotest

type exit = unit -> unit

let push l v = l := v :: !l

let wrap_test handle_result (name, s, test) =
  let test () =
    try
      test ();
      Junit.Testcase.pass
        ~name
        ~classname:""
        ~time:0.
      |> handle_result
    with
    | Failure exn_msg as exn ->
      Junit.Testcase.failure
        ~name
        ~classname:""
        ~time:0.
        ~typ:"not expected result"
        ~message:"test failed"
        exn_msg
      |> handle_result;
      raise exn
    | exn ->
      let exn_msg = Printexc.to_string exn in
      Junit.Testcase.error
        ~name
        ~classname:""
        ~time:0.
        ~typ:"exception raised"
        ~message:"test crashed"
        exn_msg
      |> handle_result;
      raise exn
  in
  (name, s, test)

let run ?argv name tl =
  A.run ~and_exit:false ?argv name tl

let run_and_report ?(and_exit=true) ?package ?timestamp ?argv name tests =
  let testcases = ref [] in
  let testsuite = Junit.Testsuite.make ?package ?timestamp ~name () in
  let tests =
    List.map (fun (title, test_set) ->
      (title, List.map (wrap_test (push testcases)) test_set)
    ) tests
  in
  let exit =
    try
      run ?argv name tests;
      fun () -> if and_exit then exit 0 else ()
    with A.Test_error ->
    fun () -> if and_exit then exit 1 else raise A.Test_error
  in
  Junit.Testsuite.add_testcases !testcases testsuite, exit
