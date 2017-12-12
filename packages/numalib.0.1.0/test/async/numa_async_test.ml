open Core
open Async
open Numalib_async

let run () =
  match%bind Numa.available () with
  | false -> 
    printf "No numa\n%!";
    Deferred.unit
  | true ->
    let%bind max_possible_node = Numa.max_possible_node () in
    let%bind num_possible_nodes = Numa.num_possible_nodes () in
    let%bind max_node = Numa.max_node () in
    let%bind num_configured_nodes = Numa.num_configured_nodes () in
    let%bind num_possible_cpus = Numa.num_possible_cpus () in
    let%bind num_configured_cpus = Numa.num_configured_cpus () in
    let%bind num_task_cpus = Numa.num_task_cpus () in
    let%bind num_task_nodes = Numa.num_task_nodes () in
    let%bind node_size64 = Numa.node_size64 ~node:0 in
    let%bind node_size = Numa.node_size ~node:0 in
    let%bind pagesize = Numa.pagesize () in
    printf "max_possible_node: %d\n%!" max_possible_node;
    printf "num_possible_nodes: %d\n%!" num_possible_nodes;
    printf "max_node: %d\n%!" max_node;
    printf "num_configured_nodes: %d\n%!" num_configured_nodes;
    printf "num_possible_cpus: %d\n%!" num_possible_cpus;
    printf "num_configured_cpus: %d\n%!" num_configured_cpus;
    printf "num_task_cpus: %d\n%!" num_task_cpus;
    printf "num_task_nodes: %d\n%!" num_task_nodes;
    printf "node_size64: %d\n%!" node_size64;
    printf "node_size: %d\n%!" node_size;
    printf "pagesize: %d\n%!" pagesize;
    let%bind mems = Numa.get_mems_allowed () in
    printf "get_mems_allowed: "; List.iter mems ~f:(printf " %d"); printf "\n%!";
    let%bind cpus = Numa.parse_cpustring "1-5" in
    printf "parse_cpustring: "; List.iter cpus ~f:(printf " %d"); printf "\n%!";
    printf "CPUs:";
    let%bind cpus = Numa.node_to_cpus ~node:0 in List.iter cpus ~f:(printf " %d");
    printf "\n%!";
    let pid = Unix.getpid () |> Pid.to_int in
    let%bind cpus = Numa.get_affinity ~pid in
    printf "get_affinity: "; List.iter cpus ~f:(printf " %d"); printf "\n%!";
    let%bind cpus = Numa.parse_cpustring "1,3,5,8-10" in
    printf "set_affinity: "; List.iter cpus ~f:(printf " %d"); printf "\n%!";
    let%bind () = Numa.set_affinity ~pid ~cpus in
    let%bind cpus = Numa.get_affinity ~pid in
    printf "get_affinity post set: "; List.iter cpus ~f:(printf " %d"); printf "\n%!";
    let%bind nodes = Numa.get_interleave_mask () in
    printf "get_interleave_mask: "; List.iter nodes ~f:(printf " %d"); printf "\n%!";
    let%bind () = Numa.set_interleave_mask ~nodes:[0;1] in
    let%bind nodes = Numa.get_interleave_mask () in
    printf "get_interleave_mask post set: "; List.iter nodes ~f:(printf " %d"); printf "\n%!";
    let%bind mbind = Numa.get_membind () in
    printf "get_membind: "; List.iter mbind ~f:(printf " %d"); printf "\n%!";
    let%bind () = Numa.set_membind ~nodes:[0] in
    let%bind mbind = Numa.get_membind () in
    printf "get_membind (post set): "; List.iter mbind ~f:(printf " %d"); printf "\n%!";
    let%bind rnodes = Numa.get_run_node_mask () in
    printf "get_run_node_mask: "; List.iter rnodes ~f:(printf " %d"); printf "\n%!";
    let%bind () = Numa.run_on_node_mask ~nodes:[0] in
    let%bind rnodes = Numa.get_run_node_mask () in
    printf "get_run_node_mask(post set): "; List.iter rnodes ~f:(printf " %d"); printf "\n%!";
    printf "running GC...\n";
    Gc.full_major ();
    printf "Done GC\n";
    Deferred.unit

let () =
  Command.async
    ~summary:"async numa test"
    Command.Spec.(
      empty
    )
    (fun () -> run ())
  |> Command.run

