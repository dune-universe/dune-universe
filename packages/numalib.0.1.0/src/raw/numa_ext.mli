type bitmask
type nodemask

external numa_bitmask_isbitset : bitmask -> int -> int
  = "caml_numa_bitmask_isbitset"
external numa_bitmask_setall : bitmask -> bitmask
  = "caml_numa_bitmask_setall"
external numa_bitmask_clearall : bitmask -> bitmask
  = "caml_numa_bitmask_clearall"
external numa_bitmask_setbit : bitmask -> int -> bitmask
  = "caml_numa_bitmask_setbit"
external numa_bitmask_clearbit : bitmask -> int -> bitmask
  = "caml_numa_bitmask_clearbit"
external numa_bitmask_nbytes : bitmask -> int = "caml_numa_bitmask_nbytes"
external numa_bitmask_weight : bitmask -> int = "caml_numa_bitmask_weight"
external numa_bitmask_alloc : int -> bitmask = "caml_numa_bitmask_alloc"
external numa_bitmask_equal : bitmask -> bitmask -> int
  = "caml_numa_bitmask_equal"
external copy_nodemask_to_bitmask : nodemask -> bitmask -> unit
  = "caml_copy_nodemask_to_bitmask"
external copy_bitmask_to_nodemask : bitmask -> nodemask -> unit
  = "caml_copy_bitmask_to_nodemask"
external copy_bitmask_to_bitmask : bitmask -> bitmask -> unit
  = "caml_copy_bitmask_to_bitmask"
external nodemask_zero : nodemask -> unit = "caml_nodemask_zero"
external nodemask_equal : nodemask -> nodemask -> int = "caml_nodemask_equal"
external numa_available : unit -> int = "caml_numa_available"
external numa_max_node : unit -> int = "caml_numa_max_node"
external numa_max_possible_node : unit -> int = "caml_numa_max_possible_node"
external numa_preferred : unit -> int = "caml_numa_preferred"
external numa_node_size64 : int -> int = "caml_numa_node_size64"
external numa_node_size : int -> int = "caml_numa_node_size"
external numa_pagesize : unit -> int = "caml_numa_pagesize"
external numa_bind : bitmask -> unit = "caml_numa_bind"
external numa_set_interleave_mask : bitmask -> unit
  = "caml_numa_set_interleave_mask"
external numa_get_interleave_mask : unit -> bitmask
  = "caml_numa_get_interleave_mask"
external numa_allocate_nodemask : unit -> bitmask
  = "caml_numa_allocate_nodemask"
external numa_set_preferred : int -> unit = "caml_numa_set_preferred"
external numa_set_localalloc : unit -> unit = "caml_numa_set_localalloc"
external numa_set_membind : bitmask -> unit = "caml_numa_set_membind"
external numa_get_membind : unit -> bitmask = "caml_numa_get_membind"
external numa_get_mems_allowed : unit -> bitmask
  = "caml_numa_get_mems_allowed"
external numa_get_interleave_node : unit -> int
  = "caml_numa_get_interleave_node"
external numa_run_on_node_mask : bitmask -> int
  = "caml_numa_run_on_node_mask"
external numa_run_on_node_mask_all : bitmask -> int
  = "caml_numa_run_on_node_mask_all"
external numa_run_on_node : int -> int = "caml_numa_run_on_node"
external numa_get_run_node_mask : unit -> bitmask
  = "caml_numa_get_run_node_mask"
external numa_set_bind_policy : int -> unit = "caml_numa_set_bind_policy"
external numa_set_strict : int -> unit = "caml_numa_set_strict"
external numa_num_possible_nodes : unit -> int
  = "caml_numa_num_possible_nodes"
external numa_num_possible_cpus : unit -> int = "caml_numa_num_possible_cpus"
external numa_num_configured_nodes : unit -> int
  = "caml_numa_num_configured_nodes"
external numa_num_configured_cpus : unit -> int
  = "caml_numa_num_configured_cpus"
external numa_num_task_cpus : unit -> int = "caml_numa_num_task_cpus"
external numa_num_task_nodes : unit -> int = "caml_numa_num_task_nodes"
external numa_num_thread_nodes : unit -> int = "caml_numa_num_thread_nodes"
external numa_allocate_cpumask : unit -> bitmask
  = "caml_numa_allocate_cpumask"
external numa_node_to_cpus : int -> bitmask -> int = "caml_numa_node_to_cpus"
external numa_node_of_cpu : int -> int = "caml_numa_node_of_cpu"
external numa_distance : int -> int -> int = "caml_numa_distance"
external numa_migrate_pages : int -> bitmask -> bitmask -> int
  = "caml_numa_migrate_pages"
external numa_sched_getaffinity : int -> bitmask -> int
  = "caml_numa_sched_getaffinity"
external numa_sched_setaffinity : int -> bitmask -> int
  = "caml_numa_sched_setaffinity"
external numa_parse_nodestring : string -> bitmask
  = "caml_numa_parse_nodestring"
external numa_parse_nodestring_all : string -> bitmask
  = "caml_numa_parse_nodestring_all"
external numa_parse_cpustring : string -> bitmask
  = "caml_numa_parse_cpustring"
external numa_parse_cpustring_all : string -> bitmask
  = "caml_numa_parse_cpustring_all"
