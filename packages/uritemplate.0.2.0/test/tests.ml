open OUnit2

let _ = run_test_tt_main ("uritemplate" >::: [
    Uritemplate_test.test_fixture;
    Template_test.test_fixture;
  ])
