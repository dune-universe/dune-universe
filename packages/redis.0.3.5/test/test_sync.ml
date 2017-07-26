module Test_sync = Test.Make(Redis_sync.Client)
module Test_sync_cluster = Test.Make(Redis_sync.ClusterClient)

let _ =
  [
    (Test_sync.test, "sync simple");
    (Test_sync_cluster.test, "sync cluster")
  ]
  |> List.map (fun (t, name) -> t name)
  |> List.fold_left max 0
  |> Pervasives.exit
