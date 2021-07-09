val limits_sampling_rate : float
val global_limit : int ref
val limit_global_memory : f:(unit -> 'a) -> ('a, exn) Result.t
val limit_allocations :
  limit:Int64.t -> f:(unit -> 'a) -> ('a * Int64.t, exn) Result.t
val max_allocation_limit : Int64.t
val limit_with_token : token:Token.t -> f:(unit -> 'a) -> ('a, exn) Result.t
val callback : sampling_rate:float -> Gc.Memprof.allocation -> unit
val is_interrupted : unit -> bool
