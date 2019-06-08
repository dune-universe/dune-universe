open Raw_spacetime_lib

val direct_call_count : _ Trace.OCaml.Direct_call_point.t -> int option

val indirect_call_count : Trace.OCaml.Indirect_call_point.Callee.t -> int option

val has_call_counts : Heap_snapshot.Series.t -> bool

val map_file :
  Unix.file_descr -> ?pos:int64 -> ('a, 'b) Bigarray.kind -> 'c Bigarray.layout ->
  bool -> int -> ('a, 'b, 'c) Bigarray.Array1.t
