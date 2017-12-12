module type IO = Numalib_raw.Io_intf.S

module Numa(IO : IO) : Numa_intf.S with module IO := IO = struct
  open IO

  module Numa = Numalib_raw.Numa_monad(IO)

  let bits_of_bitmask bitmask =
    Numa.numa_bitmask_nbytes ~bitmask
    >>= fun bytes ->
      let maxbits = bytes * 8 in
      let rec loop bit l =
        match bit < maxbits with
        | true ->
          Numa.numa_bitmask_isbitset ~bitmask ~bit
          >>= fun b -> if b <> 0 then loop (bit + 1) (bit::l) else loop (bit + 1) l
        | false ->
          List.rev l |> return
      in
      loop 0 []

  let bits_to_bitmask bits = 
    let maxbits = List.fold_left (fun max b -> if b > max then b else max) 1 bits in
    Numa.numa_bitmask_alloc ~bits:(maxbits + 1)
    >>= fun bitmask ->
      let rec loop bitmask = function
        | [] -> return bitmask
        | bit::tl ->
          Numa.numa_bitmask_setbit ~bitmask ~bit
          >>= fun bitmask -> loop bitmask tl
      in
      loop bitmask bits

  let node_to_cpus ~node =
    Numa.numa_allocate_cpumask ()
    >>= fun bitmask ->
    Numa.numa_node_to_cpus ~node ~bitmask
    >>= fun res ->
    if res < 0 then raise (Invalid_argument "mask not large enough")
    else bits_of_bitmask bitmask

  let available () = Numa.numa_available () >>= fun i -> return (i >= 0)
  let max_possible_node () = Numa.numa_max_possible_node ()
  let num_possible_nodes () = Numa.numa_num_possible_nodes ()
  let num_possible_cpus () = Numa.numa_num_possible_cpus ()
  let max_node () = Numa.numa_max_node ()
  let num_configured_nodes () = Numa.numa_num_configured_nodes ()
  let num_configured_cpus () = Numa.numa_num_configured_cpus ()
  let num_task_cpus () = Numa.numa_num_task_cpus ()
  let num_task_nodes () = Numa.numa_num_task_nodes ()
  let get_mems_allowed () = Numa.numa_get_mems_allowed () >>= bits_of_bitmask
  let parse_cpustring s = Numa.numa_parse_cpustring s >>= bits_of_bitmask
  let parse_nodestring s = Numa.numa_parse_nodestring s >>= bits_of_bitmask
  let node_of_cpu ~cpu = Numa.numa_node_of_cpu cpu
  let node_distance node1 node2 = Numa.numa_distance node1 node2

  let get_affinity ~pid =
    Numa.numa_allocate_cpumask ()
    >>= fun bitmask ->
    Numa.numa_sched_getaffinity ~pid ~bitmask
    >>= fun ret ->
    if ret < 0 then raise (Failure "numa_sched_get_affinity failed")
    else bits_of_bitmask bitmask

  let set_affinity ~pid ~cpus =
    bits_to_bitmask cpus
    >>= fun bitmask ->
    Numa.numa_sched_setaffinity ~pid ~bitmask
    >>= fun ret ->
    if ret < 0 then raise (Failure "numa_sched_set_affinity failed")
    else return ()

  let preferred_node () = Numa.numa_preferred ()
  let set_preferred_node ~node = Numa.numa_set_preferred node

  let node_size64 ~node = Numa.numa_node_size64 node
  let node_size ~node = Numa.numa_node_size node

  let pagesize () = Numa.numa_pagesize ()
  let set_strict ~strict =
    if strict then Numa.numa_set_strict ~strict:1 else Numa.numa_set_strict ~strict:0

  let get_interleave_mask () = Numa.numa_get_interleave_mask () >>= bits_of_bitmask

  let set_interleave_mask ~nodes =
    bits_to_bitmask nodes
    >>= fun bitmask ->
    Numa.numa_set_interleave_mask bitmask

  let bind ~nodes =
    bits_to_bitmask nodes
    >>= fun nodemask ->
    Numa.numa_bind nodemask

  let set_localalloc () = Numa.numa_set_localalloc ()

  let set_membind ~nodes = 
    bits_to_bitmask nodes
    >>= fun nodemask -> Numa.numa_set_membind nodemask

  let get_membind () = Numa.numa_get_membind () >>= bits_of_bitmask

  let run_on_node_mask ~nodes =
    bits_to_bitmask nodes
    >>= fun nodemask -> Numa.numa_run_on_node_mask nodemask
    >>= fun ret ->
    if ret < 0 then raise (Failure "numa_run_on_node_mask failed")
    else return ()

  let run_on_node ~node = Numa.numa_run_on_node node
    >>= fun ret ->
    if ret < 0 then raise (Failure "run_on_node failed")
    else return ()

  let get_run_node_mask () = Numa.numa_get_run_node_mask () >>= bits_of_bitmask

  let set_bind_policy ~strict =
    let strict = if strict then 1 else 0 in
    Numa.numa_set_bind_policy ~strict
end
