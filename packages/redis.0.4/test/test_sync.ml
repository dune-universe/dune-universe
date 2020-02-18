module Test_sync = Test.Make(Redis_sync.Client)
module Test_sync_cluster = Test.Make(Redis_sync.ClusterClient)
open OUnit

let suite =
  "sync" >::: [
    Test_sync.suite "simple";
    Test_sync_cluster.suite "cluster";
  ]

let () =
  Random.self_init ();
  let res = run_test_tt suite in
  Test_sync.teardown ();
  exit @@ Test.test_exit_code res
