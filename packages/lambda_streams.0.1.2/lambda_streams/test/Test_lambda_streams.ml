let () =
  print_newline ();
  Alcotest.run
    "Lambda_streams"
    [
      "Signal", Test_Signal.suite;
      "Finite.Sync", Test_Finite_Sync.suite;
      "Sync", Test_Sync.suite;
    ]
