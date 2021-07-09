val started_1 : bool ref
val started_2 : bool Atomic.t
val check_memprof_limits_started : string -> unit
val start_2 : unit -> unit
val start_1 :
  sampling_rate:float ->
  ?callstack_size:int -> ('a, 'b) Gc.Memprof.tracker -> unit
val stop_1 : unit -> unit
