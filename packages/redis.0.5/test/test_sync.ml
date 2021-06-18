module Utils = struct
  module IO = Redis_sync.Client.IO
  let spawn f ~on_complete =
    let _th = Thread.create (fun () ->
        let x = f() in
        on_complete x) ()
    in
    ()
end
module Test_sync = Test.Make(Redis_sync.Client)(Utils)
module Test_sync_cluster = Test.Make(Redis_sync.ClusterClient)(Utils)
open OUnit2

let suite =
  "sync" >::: [
    Test_sync.suite "simple";
    Test_sync_cluster.suite "cluster";
  ]

let () =
  Random.self_init ();
  let code = ref 0 in
  OUnit2.run_test_tt_main ~exit:(fun i -> code := i) suite;
  Test_sync.teardown ();
  exit !code
