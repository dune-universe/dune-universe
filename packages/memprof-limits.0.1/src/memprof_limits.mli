val with_memprof_limits : (unit -> 'a) -> 'a
(** Turn on the memprof-limits engine for the duration of a scope,
    e.g. at the start of your program. Simply doing so should have no
    impact on performance. This is necessary before using any of the
    functions below. *)

type 'a result = ('a, exn) Result.t
(** A result is either the return value of a successful computation,
    or, in case the computation has been interrupted, a (private)
    exception raised by memprof-limits or any other exception (bug,
    uncaught exception, or other interrupt) that arose during clean-up
    after the first exception was raised. (This exception can be
    discarded.)

    Once interrupted, the exception will keep being raised by
    memprof-limits regularly until a result is produced.

    Any exception raised in the absence of an interruption by the
    corresponding limit is unaffected and arises normally.

    All functions producing a [result] raise [Failure] if the engine
    is not started. *)


(** 1. Per-thread global memory limit *)

val with_global_memory_limit : (unit -> 'a) -> 'a result
(** Executes the argument function under the threat of being
    interrupted if the global memory (major heap word count) exceeds
    the limit.

    If interrupted, then the program is likely in a state where
    subsequent checks of memory limits will result in further
    interrupts being raised in other monitored parts of the program.
    You can either let other monitored threads be interrupted too until
    the situation is resolved (for instance by the program shutting
    down), or you can call [Gc.compact ()] to attempt to free up space.
    The chance for a monitored thread to be interrupted relative to
    others is proportional to the allocation rate. *)

val set_global_memory_limit : int -> unit
(** Set the limit in KiB for the major heap word count over which one
    or more threads (selected with {!with_global_memory_limit}) will be
    interrupted. A special value of 0 disables the limit.

    Default: 0 *)


(** 1. Per-thread allocation limits *)

val with_allocation_limit : limit:Int64.t -> (unit -> 'a) -> 'a result
(** Executes the argument function under the threat of being
    interrupted if it allocates more than the limit (in KiB). It counts
    allocations (approximately) but not deallocations; as such it is a
    measure of work done which is more portable than wall clock, and
    can be more suitable (for instance it does not count system calls).

    Refer to the documentation for the accuracy of the counting method
    and for advice about good values for [limit].

    If two calls to [with_allocation_limit] are nested, then
    allocations are counted for both. A call to [with_allocation_limit]
    cannot be used to go beyond a previous limit. *)

val max_allocation_limit : Int64.t
(** The maximal value for [~limit], which is equal to
    72'057'594'037'927'935 KiB. *)

(*
(** 1. Non-memory limits *)

val with_limit : limit_reached:(unit -> bool) -> (unit -> 'a) -> 'a result
(** Executes the argument function under the threat of being
    interrupted if [limit_reached] returns true. If [limit_reached] has
    previously returned true for that instance of limit, then it is
    remembered and the limit will keep arising.

    For instance, it can be combined with a signal handler to record
    that a signal arrived and interrupt more than one computation.

    Mildly reliable in code that allocates little. *)
*)

(** 1. Profiling *)

(** Use the following reimplementation of the Memprof interface for
    profiling needs.

    Remark: the expectancy (1 / [sampling_rate]) you provide in
    [Memprof.start] gets rounded to the nearest integer for the sake of
    the counting accuracy of memprof-limits. *)
module Memprof : module type of Stdlib.Gc.Memprof
