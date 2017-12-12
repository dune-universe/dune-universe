module Numa(IO : Io_intf.S) : Numa_intf.S with module IO := IO = struct
  type bitmask = Numa_ext.bitmask
  type nodemask = Numa_ext.nodemask

  let wrap = IO.wrap

  let numa_bitmask_isbitset ~bitmask ~bit =
    wrap (Numa_ext.numa_bitmask_isbitset bitmask) bit

  let numa_bitmask_setall ~bitmask = wrap Numa_ext.numa_bitmask_setall bitmask
  let numa_bitmask_clearall ~bitmask = wrap Numa_ext.numa_bitmask_clearall bitmask

  let numa_bitmask_setbit ~bitmask ~bit
    = wrap (Numa_ext.numa_bitmask_setbit bitmask) bit

  let numa_bitmask_clearbit ~bitmask ~bit =
    wrap (Numa_ext.numa_bitmask_clearbit bitmask) bit

  let numa_bitmask_nbytes ~bitmask = wrap Numa_ext.numa_bitmask_nbytes bitmask
  let numa_bitmask_weight ~bitmask = wrap Numa_ext.numa_bitmask_weight bitmask
  let numa_bitmask_alloc ~bits  = wrap Numa_ext.numa_bitmask_alloc bits
  let numa_bitmask_equal ~mask1 ~mask2 = wrap (Numa_ext.numa_bitmask_equal mask1) mask2

  let copy_nodemask_to_bitmask ~nodemask ~bitmask =
    wrap (Numa_ext.copy_nodemask_to_bitmask nodemask) bitmask

  let copy_bitmask_to_nodemask ~bitmask ~nodemask =
    wrap (Numa_ext.copy_bitmask_to_nodemask bitmask) nodemask

  let copy_bitmask_to_bitmask ~srcmask ~dstmask =
    wrap (Numa_ext.copy_bitmask_to_bitmask srcmask) dstmask

  let nodemask_zero nodemask  = wrap Numa_ext.nodemask_zero nodemask
  let nodemask_equal nodemask1 nodemask2 =
    wrap (Numa_ext.nodemask_equal nodemask1) nodemask2

  let numa_available () = wrap Numa_ext.numa_available ()
  let numa_max_node () = wrap Numa_ext.numa_max_node ()
  let numa_max_possible_node () = wrap Numa_ext.numa_max_possible_node ()
  let numa_preferred () = wrap Numa_ext.numa_preferred ()
  let numa_node_size64 node = wrap Numa_ext.numa_node_size64 node
  let numa_node_size node = wrap Numa_ext.numa_node_size node
  let numa_pagesize () = wrap Numa_ext.numa_pagesize ()
  let numa_bind bitmask = wrap Numa_ext.numa_bind bitmask
  let numa_set_interleave_mask bitmask = wrap Numa_ext.numa_set_interleave_mask bitmask
  let numa_get_interleave_mask () = wrap Numa_ext.numa_get_interleave_mask ()
  let numa_allocate_nodemask () = wrap Numa_ext.numa_allocate_nodemask ()
  let numa_set_preferred node = wrap Numa_ext.numa_set_preferred node
  let numa_set_localalloc () = wrap Numa_ext.numa_set_localalloc ()
  let numa_set_membind bitmask = wrap Numa_ext.numa_set_membind bitmask
  let numa_get_membind () = wrap Numa_ext.numa_get_membind ()
  let numa_get_mems_allowed () = wrap Numa_ext.numa_get_mems_allowed ()
  let numa_get_interleave_node () = wrap Numa_ext.numa_get_interleave_node ()
  let numa_run_on_node_mask bitmask = wrap Numa_ext.numa_run_on_node_mask bitmask
  let numa_run_on_node_mask_all bitmask = wrap Numa_ext.numa_run_on_node_mask_all bitmask
  let numa_run_on_node node = wrap Numa_ext.numa_run_on_node node
  let numa_get_run_node_mask () = wrap Numa_ext.numa_get_run_node_mask ()
  let numa_set_bind_policy ~strict = wrap Numa_ext.numa_set_bind_policy strict
  let numa_set_strict ~strict = wrap Numa_ext.numa_set_strict strict
  let numa_num_possible_nodes () = wrap Numa_ext.numa_num_possible_nodes ()
  let numa_num_possible_cpus () = wrap Numa_ext.numa_num_possible_cpus ()
  let numa_num_configured_nodes () = wrap Numa_ext.numa_num_configured_nodes ()
  let numa_num_configured_cpus () = wrap Numa_ext.numa_num_configured_cpus ()
  let numa_num_task_cpus () = wrap Numa_ext.numa_num_task_cpus ()
  let numa_num_task_nodes () = wrap Numa_ext.numa_num_task_nodes ()
  let numa_num_thread_nodes () = wrap Numa_ext.numa_num_thread_nodes ()
  let numa_allocate_cpumask () = wrap Numa_ext.numa_allocate_cpumask ()
  let numa_node_to_cpus ~node ~bitmask = wrap (Numa_ext.numa_node_to_cpus node) bitmask
  let numa_node_of_cpu cpu = wrap Numa_ext.numa_node_of_cpu cpu
  let numa_distance node1 node2 = wrap (Numa_ext.numa_distance node1) node2

  let numa_migrate_pages ~pid ~fromnodes ~tonodes =
    wrap (Numa_ext.numa_migrate_pages pid fromnodes) tonodes

  let numa_sched_getaffinity ~pid ~bitmask =
    wrap (Numa_ext.numa_sched_getaffinity pid) bitmask

  let numa_sched_setaffinity ~pid ~bitmask =
    wrap (Numa_ext.numa_sched_setaffinity pid) bitmask

  let numa_parse_nodestring node_s = wrap Numa_ext.numa_parse_nodestring node_s
  let numa_parse_nodestring_all node_s = wrap Numa_ext.numa_parse_nodestring_all node_s
  let numa_parse_cpustring cpu_s = wrap Numa_ext.numa_parse_cpustring cpu_s
  let numa_parse_cpustring_all cpu_s  = wrap Numa_ext.numa_parse_cpustring_all cpu_s

end
