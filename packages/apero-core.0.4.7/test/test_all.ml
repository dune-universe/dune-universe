
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


(* Run it *)
let () =
  setup_log (Some `None) (Some Logs.Debug);
  Printexc.record_backtrace true;
  Alcotest.run "Resource Test" [
    "test_resource", Test_resource.all_tests;
    "test_vle", Test_vle.all_tests;
  ]