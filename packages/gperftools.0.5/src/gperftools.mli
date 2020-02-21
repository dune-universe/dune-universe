(**
  Bindings to gperftools. Monitor memory usage and control behaviour of tcmalloc.

  @see <https://github.com/gperftools/gperftools> gperftools project page

  @author ygrek\@autistici.org
*)

(** {2 malloc interface} *)

(** Release as much memory as possible to operating system *)
val release_free_memory : unit -> unit

(** Sets the rate at which we release unused memory to the system.
  Zero means we never release memory back to the system.  Increase
  this flag to return memory faster; decrease it to return memory
  slower.  Reasonable rates are in the range \[0,10\].  (Currently
  only implemented in tcmalloc). *)
val set_memory_release_rate : float -> unit

(** Gets the release rate.  Returns a value < 0 if unknown. *)
val get_memory_release_rate : unit -> float

exception Invalid_property of string

(** @raise Invalid_property if the property is not a valid property name for the
  current malloc implementation, or is not writable *)
val set_numeric_property : string -> int -> unit

(** @raise Invalid_property if the property is not a valid property name for the
  current malloc implementation *)
val get_numeric_property : string -> int

(**
Some currently useful properties :
- [generic.current_allocated_bytes] - number of bytes used by the application. This will not typically match the memory use reported by the OS, because it does not include TCMalloc overhead or memory fragmentation.
- [generic.heap_size] - bytes of system memory reserved by TCMalloc.
- [tcmalloc.pageheap_free_bytes] - number of bytes in free, mapped pages in page heap. These bytes can be used to fulfill allocation requests. They always count towards virtual memory usage, and unless the underlying memory is swapped out by the OS, they also count towards physical memory usage.
- [tcmalloc.pageheap_unmapped_bytes] - number of bytes in free, unmapped pages in page heap. These are bytes that have been released back to the OS, possibly by one of the MallocExtension "Release" calls. They can be used to fulfill allocation requests, but typically incur a page fault. They always count towards virtual memory usage, and depending on the OS, typically do not count towards physical memory usage.
- [tcmalloc.max_total_thread_cache_bytes] - a limit to how much memory TCMalloc dedicates for small objects. Higher numbers trade off more memory use for -- in some situations -- improved efficiency. Writable.
- [tcmalloc.current_total_thread_cache_bytes] - a measure of some of the memory TCMalloc is using (for small objects).
- [tcmalloc.aggressive_memory_decommit] - when returning chunk of memory to system, try to return all neighboring free chunks as well. Writable.
*)
val known_properties : string list

(* @return version information: (human-readable string, major, minor, patch level) *)
val version : unit -> string * int * int * string

(** {2 Heap profiler} *)

val heap_profiler_start : string -> unit
val is_heap_profiler_running : unit -> bool
val heap_profiler_stop : unit -> unit
val heap_profiler_dump : string -> unit
val get_heap_profile : unit -> string
