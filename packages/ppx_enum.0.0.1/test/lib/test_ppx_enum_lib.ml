open OUnit2

let suite =
  "ppx_enum_lib" >:::
  [ Test_utils.suite
  ]

let () = run_test_tt_main suite
