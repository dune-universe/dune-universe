let () =
  Lwt_main.run @@ Alcotest_lwt.run "Main tests" [
  "utility test", Test_utility.utility_test;
  "integration test", Integration_test.test;
  "partition key test", Integration_test.test_partition_key;
]
