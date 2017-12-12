module type S = sig
  module IO : Io_intf.S

  type bitmask
  type nodemask

  val numa_bitmask_isbitset : bitmask:bitmask -> bit:int -> int IO.t
  val numa_bitmask_setall : bitmask:bitmask -> bitmask IO.t
  val numa_bitmask_clearall : bitmask:bitmask -> bitmask IO.t
  val numa_bitmask_setbit : bitmask:bitmask -> bit:int -> bitmask IO.t
  val numa_bitmask_clearbit : bitmask:bitmask -> bit:int -> bitmask IO.t
  val numa_bitmask_nbytes : bitmask:bitmask -> int IO.t
  val numa_bitmask_weight : bitmask:bitmask -> int IO.t
  val numa_bitmask_alloc : bits:int -> bitmask IO.t
  val numa_bitmask_equal : mask1:bitmask -> mask2:bitmask -> int IO.t
  val copy_nodemask_to_bitmask : nodemask:nodemask -> bitmask:bitmask -> unit IO.t
  val copy_bitmask_to_nodemask : bitmask:bitmask -> nodemask:nodemask -> unit IO.t
  val copy_bitmask_to_bitmask : srcmask:bitmask -> dstmask:bitmask -> unit IO.t
  val nodemask_zero : nodemask -> unit IO.t
  val nodemask_equal : nodemask -> nodemask -> int IO.t
  val numa_available : unit -> int IO.t
  val numa_max_node : unit -> int IO.t
  val numa_max_possible_node : unit -> int IO.t
  val numa_preferred : unit -> int IO.t
  val numa_node_size64 : int -> int IO.t
  val numa_node_size : int -> int IO.t
  val numa_pagesize : unit -> int IO.t
  val numa_bind : bitmask -> unit IO.t
  val numa_set_interleave_mask : bitmask -> unit IO.t
  val numa_get_interleave_mask : unit -> bitmask IO.t
  val numa_allocate_nodemask : unit -> bitmask IO.t
  val numa_set_preferred : int -> unit IO.t
  val numa_set_localalloc : unit -> unit IO.t
  val numa_set_membind : bitmask -> unit IO.t
  val numa_get_membind : unit -> bitmask IO.t
  val numa_get_mems_allowed : unit -> bitmask IO.t
  val numa_get_interleave_node : unit -> int IO.t
  val numa_run_on_node_mask : bitmask -> int IO.t
  val numa_run_on_node_mask_all : bitmask -> int IO.t
  val numa_run_on_node : int -> int IO.t
  val numa_get_run_node_mask : unit -> bitmask IO.t
  val numa_set_bind_policy : strict:int -> unit IO.t
  val numa_set_strict : strict:int -> unit IO.t
  val numa_num_possible_nodes : unit -> int IO.t
  val numa_num_possible_cpus : unit -> int IO.t
  val numa_num_configured_nodes : unit -> int IO.t
  val numa_num_configured_cpus : unit -> int IO.t
  val numa_num_task_cpus : unit -> int IO.t
  val numa_num_task_nodes : unit -> int IO.t
  val numa_num_thread_nodes : unit -> int IO.t
  val numa_allocate_cpumask : unit -> bitmask IO.t
  val numa_node_to_cpus : node:int -> bitmask:bitmask -> int IO.t
  val numa_node_of_cpu : int -> int IO.t
  val numa_distance : int -> int -> int IO.t
  val numa_migrate_pages : pid:int -> fromnodes:bitmask -> tonodes:bitmask -> int IO.t
  val numa_sched_getaffinity : pid:int -> bitmask:bitmask -> int IO.t
  val numa_sched_setaffinity : pid:int -> bitmask:bitmask -> int IO.t
  val numa_parse_nodestring : string -> bitmask IO.t
  val numa_parse_nodestring_all : string -> bitmask IO.t
  val numa_parse_cpustring : string -> bitmask IO.t
  val numa_parse_cpustring_all : string -> bitmask IO.t
end
