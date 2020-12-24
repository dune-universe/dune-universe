
let () =
  Lwt_main.run @@
    Alcotest_lwt.run
      "Lwt Nbd library test suite"
      (* I found that running the protocol tests before the
         client server tests causes the test suite to hang
       *)
      [ Client_server_test.tests
      ; Protocol_test.tests
      ];
  Alcotest.run
    "Sync Nbd library test suite"
    [ Mux_test.tests
    ]
