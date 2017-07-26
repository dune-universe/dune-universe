open OUnit2

let () =
  [
    BitstringConstructorTest.suite ;
    BitstringParserTest.suite      ;
    BitstringQualifierTest.suite   ;
  ]
  |> List.iter (fun t -> run_test_tt_main t)
