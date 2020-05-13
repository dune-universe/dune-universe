val limits_sampling_rate : float
val global_limit : int ref
val with_global_memory_limit : (unit -> 'a) -> ('a, exn) Result.t
val with_allocation_limit : limit:Int64.t -> (unit -> 'a) -> ('a, exn) Result.t
val max_allocation_limit : Int64.t
(* val with_limit : limit_reached:(unit -> bool) -> (unit -> 'a) -> ('a, exn) Result.t *)
val callback : sampling_rate:float -> unit
val reset : unit -> unit
