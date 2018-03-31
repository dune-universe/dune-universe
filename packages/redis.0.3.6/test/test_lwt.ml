module Test_lwt = Test.Make(Redis_lwt.Client)
module Test_lwt_cluster = Test.Make(Redis_lwt.ClusterClient)

let _ =
  [
    (Test_lwt.test, "lwt simple");
    (Test_lwt_cluster.test, "lwt cluster")
  ]
  |> List.map (fun (t, name) -> t name)
  |> List.fold_left max 0
  |> Pervasives.exit
