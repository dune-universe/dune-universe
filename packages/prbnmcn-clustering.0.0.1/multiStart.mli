(** [multi_start] performs [nstarts] independent calls to the given function [f], which
    must return some outcome together with its cost. [multi_start] then returns the result
    that has minimal cost. [multi_start_parallel] does the same by forking several
    processes (in the hope of getting some parallelism). Note: for the parallel version,
    the random state is reinitizalized using Random.self_init per process.
*)
val multi_start : f:(unit -> float * 'a) -> nstarts:int -> float * 'a

(* val multi_start_parallel : f:(unit -> float * 'a) -> nstarts:int -> ncores:int -> float * 'a *)
