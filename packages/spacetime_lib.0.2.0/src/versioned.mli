open Raw_spacetime_lib

val direct_call_count : _ Trace.OCaml.Direct_call_point.t -> int option

val indirect_call_count : Trace.OCaml.Indirect_call_point.Callee.t -> int option

val has_call_counts : Heap_snapshot.Series.t -> bool
