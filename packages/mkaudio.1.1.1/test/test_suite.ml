open OUnit2

let suite =
  "base suite" >::: [
    Test_beat.suite;
    Test_time.suite;
  ]

let () = run_test_tt_main suite
