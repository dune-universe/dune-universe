open Numalib_raw

module type S = sig
  module IO : Io_intf.S

  (** [node_to_cpus] converts a node number, [node], to a list of CPUs
      hosted on the node *)
  val node_to_cpus : node:int -> int list IO.t

  (** [available] - true if NUMA is supported on the platform *)
  val available : unit -> bool IO.t

  (** [max_possible_node] - the highest possible node in the system *)
  val max_possible_node : unit -> int IO.t

  (** [num_possible_nodes] - the size of the kernels node mask *)
  val num_possible_nodes : unit -> int IO.t

  (** [num_possible_cpus] - the maximum number of CPUs *)
  val num_possible_cpus : unit -> int IO.t

  (** [max_node] - highest node number available on the system *)
  val max_node : unit -> int IO.t

  (** [num_configured_nodes] - number of configure nodes in the system *)
  val num_configured_nodes : unit -> int IO.t

  (** [num_configured_cpus] - number of configure CPUs in the system *)
  val num_configured_cpus : unit -> int IO.t

  (** [num_task_cpus] - number of CPUs the calling task can use *)
  val num_task_cpus : unit -> int IO.t

  (** [num_task_nodes] - number of nodes the calling task can use *) 
  val num_task_nodes : unit -> int IO.t

  (** [get_mems_allowed] - returns the list of 
   *)
  val get_mems_allowed : unit -> int list IO.t

  (** [parse_cpustring] - convert a CPU specification string to a bit list. The
      specification is a comma separated list of CPU numbers and ranges.
      See NUMA(3) for more details *)
  val parse_cpustring : string -> int list IO.t

  (** [parse_nodestring] - convert a node specification string to a bit list. The
      specification is a comma separated list of node numbers and ranges.
      See NUMA(3) for more details *)
  val parse_nodestring : string -> int list IO.t

  (** [node_of_cpu] returns the node the specified CPU is part of *)
  val node_of_cpu : cpu:int -> int IO.t

  (** [node_distance] returns the distance between 2 nodes *)
  val node_distance : int -> int -> int IO.t

  (** [get_affinity] returns the list of CPUs the process [pid] is bound to *)
  val get_affinity : pid:int -> int list IO.t

  (** [set_affinity] sets the CPUs process [pid] runs on *)
  val set_affinity : pid:int -> cpus:int list -> unit IO.t

  (** [preferred_node] gets the preferred node for memory allocations *)
  val preferred_node : unit -> int IO.t

  (** [set_preferred_node] sets the preferred node for memory allocations *)
  val set_preferred_node : node:int -> unit IO.t

  (** [node_size64] returns the amount of memory available on a node in bytes *)
  val node_size64 : node:int -> int IO.t

  (** [node_size] returns the amount of memory available on a node in bytes *)
  val node_size : node:int -> int IO.t

  (** [pagesize] returns the page size in bytes *)
  val pagesize : unit -> int IO.t

  (** [set_strict] enables or disables strict memory allocation. When [strict] is
      [true] memory allocation will fail if the memory is not available on the node *)
  val set_strict : strict:bool -> unit IO.t

  (** [get_interleave_mask] returns the list of nodes memory will be allocated from *)
  val get_interleave_mask : unit -> int list IO.t

  (** [set_interleave_mask] sets the list of nodes memory will be allocated from *)
  val set_interleave_mask : nodes:int list -> unit IO.t

  (** [bind] binds the process to the list of nodes specified in [nodes]. The process
      will only run on the CPUs of the specified nodes and memory will only be allocated
      from these nodes.  *)
  val bind : nodes:int list -> unit IO.t

  (** [set_localalloc] sets the memory allocation policy for the calling task to local
      allocation. In this mode, the preferred node for memory allocation is effectively
      the node where the task is executing at the time of a page allocation.  *)
  val set_localalloc : unit -> unit IO.t

  (** [set_membind] binds the process to the list of nodes specified in [nodes]. The process
      will only allocate memory on these nodes.  *)
  val set_membind : nodes:int list -> unit IO.t

  (** [get_membind] returns the list of nodes the process memory allocation is bound to *)
  val get_membind : unit -> int list IO.t

  (** [run_on_node_mask] runs the current task on the specified nodes *)
  val run_on_node_mask : nodes:int list -> unit IO.t

  (** [run_on_node] runs the current task on the specified node *)
  val run_on_node : node:int -> unit IO.t

  (** [get_run_node_mask] returns the list of nodes the process will run on *)
  val get_run_node_mask : unit -> int list IO.t

  (** [set_bind_policy] specifies whether calls that bind memory to a specific node should
      use the preferred policy or a strict policy. The preferred policy allows the kernel to
      allocate memory on other nodes when there isn't enough free on the target node. strict
      will fail the allocation in that case. Setting the argument to specifies strict, 0 preferred. *)
  val set_bind_policy : strict:bool -> unit IO.t
end
