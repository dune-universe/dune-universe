open Numalib_raw
open Numalib

let printf = Printf.printf

let raw_test () =
  printf "Raw:\n";
  let cpus = Numa_ext.numa_num_configured_cpus () in
    printf "CPUs = %d\n%!" cpus;
  let maxcpus = Numa_ext.numa_num_possible_cpus () in
    printf "Max CPUs = %d\n%!" maxcpus;
  let mask = Numa_ext.numa_allocate_cpumask () in
    match Numa_ext.numa_node_to_cpus 0 mask with
    | 0 -> 
      for i = 0 to cpus - 1 do
        if Numa_ext.numa_bitmask_isbitset mask i <> 0 then printf "%d " i;
      done;
      printf "\n%!";
    | _ as err -> printf "ERROR: %d\n%!" err

let () =
  if Numa_ext.numa_available () < 0 then begin
    printf "No numa\n%!";
    exit 255;
  end;
  raw_test ();
  printf "running GC...\n";
  Gc.full_major ();
  printf "Done GC\n";

  printf "\nMonadic:\n%!";
  printf "max_possible_node: %d\n%!" (Numa.max_possible_node ());
  printf "num_possible_nodes: %d\n%!" (Numa.num_possible_nodes ());
  printf "max_node: %d\n%!" (Numa.max_node ());
  printf "num_configured_nodes: %d\n%!" (Numa.num_configured_nodes ());
  printf "num_possible_cpus: %d\n%!" (Numa.num_possible_cpus ());
  printf "num_configured_cpus: %d\n%!" (Numa.num_configured_cpus ());
  printf "num_task_cpus: %d\n%!" (Numa.num_task_cpus ());
  printf "num_task_nodes: %d\n%!" (Numa.num_task_nodes ());
  printf "node_size64: %d\n%!" (Numa.node_size64 ~node:0);
  printf "node_size: %d\n%!" (Numa.node_size ~node:0);
  printf "pagesize: %d\n%!" (Numa.pagesize ());
  let mems = Numa.get_mems_allowed () in
  printf "get_mems_allowed: "; List.iter (printf " %d") mems; printf "\n%!";
  let cpus = Numa.parse_cpustring "1-5" in
  printf "parse_cpustring: "; List.iter (printf " %d") cpus; printf "\n%!";
  printf "CPUs:";
  let cpus = Numa.node_to_cpus ~node:0 in List.iter (printf " %d") cpus;
  printf "\n%!";
  let pid = Unix.getpid () in
  let cpus = Numa.get_affinity ~pid in
  printf "get_affinity: "; List.iter (printf " %d") cpus; printf "\n%!";
  let cpus = Numa.parse_cpustring "1,3,5,8-10" in
  printf "set_affinity: "; List.iter (printf " %d") cpus; printf "\n%!";
  let () = Numa.set_affinity ~pid ~cpus in
  let cpus = Numa.get_affinity ~pid in
  printf "get_affinity post set: "; List.iter (printf " %d") cpus; printf "\n%!";
  let nodes = Numa.get_interleave_mask () in
  printf "get_interleave_mask: "; List.iter (printf " %d") nodes; printf "\n%!";
  let () = Numa.set_interleave_mask ~nodes:[0;1] in
  let nodes = Numa.get_interleave_mask () in
  printf "get_interleave_mask post set: "; List.iter (printf " %d") nodes; printf "\n%!";
  let mbind = Numa.get_membind () in
  printf "get_membind: "; List.iter (printf " %d") mbind; printf "\n%!";
  let () = Numa.set_membind ~nodes:[0] in
  let mbind = Numa.get_membind () in
  printf "get_membind (post set): "; List.iter (printf " %d") mbind; printf "\n%!";
  let rnodes = Numa.get_run_node_mask () in
  printf "get_run_node_mask: "; List.iter (printf " %d") rnodes; printf "\n%!";
  let () = Numa.run_on_node_mask ~nodes:[0] in
  let rnodes = Numa.get_run_node_mask () in
  printf "get_run_node_mask(post set): "; List.iter (printf " %d") rnodes; printf "\n%!";
  printf "running GC...\n";
  Gc.full_major ();
  printf "Done GC\n";
