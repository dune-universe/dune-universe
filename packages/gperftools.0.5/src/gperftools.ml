
exception Invalid_property of string
let () = Callback.register_exception "Gperftools.Invalid_property" (Invalid_property "")

external release_free_memory : unit -> unit = "ml_gpt_ReleaseFreeMemory"
external set_memory_release_rate : float -> unit = "ml_gpt_SetMemoryReleaseRate"
external get_memory_release_rate : unit -> float = "ml_gpt_GetMemoryReleaseRate"
external set_numeric_property : string -> int -> unit = "ml_gpt_SetNumericProperty"
external get_numeric_property : string -> int = "ml_gpt_GetNumericProperty"

let known_properties = [
  "generic.current_allocated_bytes";
  "generic.heap_size";
  "tcmalloc.pageheap_free_bytes";
  "tcmalloc.pageheap_unmapped_bytes";
  "tcmalloc.central_cache_free_bytes";
  "tcmalloc.transfer_cache_free_bytes";
  "tcmalloc.thread_cache_free_bytes";
  "tcmalloc.max_total_thread_cache_bytes";
  "tcmalloc.current_total_thread_cache_bytes";
  "tcmalloc.aggressive_memory_decommit";
]

external version : unit -> string * int * int * string = "ml_tc_version"

external heap_profiler_start : string -> unit = "ml_gpt_HeapProfilerStart"
external is_heap_profiler_running : unit -> bool = "ml_gpt_IsHeapProfilerRunning"
external heap_profiler_stop : unit -> unit = "ml_gpt_HeapProfilerStop"
external heap_profiler_dump : string -> unit = "ml_gpt_HeapProfilerDump"
external get_heap_profile : unit -> string = "ml_gpt_GetHeapProfile"
