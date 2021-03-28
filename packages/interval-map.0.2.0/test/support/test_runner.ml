(** Main entry point for our test runner.

    This aggregates all the test suites and call Alcotes to run them. When
    creating a new test suite, don't forget to add it here! *)

let () = Alcotest.run "interval-map" [ "Interval map", Interval_map_test.suite ]
