(* Run tests *)
let _ =
  QCheck_runner.run_tests
  ( List.test_suite
  @ AVL.test_suite
  @ Map.test_suite
  @ Set.test_suite
  )
