open OUnit2

let suite =
  "ppx_factory_lib" >:::
  [ Test_util.suite
  ; Test_default.suite
  ; Test_factory.suite
  ]

let () = run_test_tt_main suite
