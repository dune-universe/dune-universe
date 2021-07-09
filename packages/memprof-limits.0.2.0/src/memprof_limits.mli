(** Global memory limits, allocation limits, and cancellation of
    CPU-bound threads.

    All the functions in this module are thread-safe. *)


(** {1:task Result of a task} *)

type 'a result = ('a, exn) Result.t
(** Denotes the result of a {i task}: a computation running in a
    thread under one of the limits described below. This result is
    either the return value produced from the the successful
    completion of the task, given by [Ok], or, in case the task has
    been interrupted due to a limit being reached, some exception
    given by [Error].

    This exception can be a (private) exception raised by
    memprof-limits, or another exception (bug, uncaught exception, or
    another interrupt) that arose after the private exception was
    raised and before the result is produced. (If the task follows the
    {{!recovering} guidelines for safely recovering from interrupts},
    this exception can be discarded.)

    Once the task is interrupted, the private exception will keep
    arising regularly inside the task until a [result] is produced.

    It is guaranteed that [Error] is produced if and only if the
    corresponding limit has been exceeded by the task. Any exception
    raised otherwise is unaffected and arises normally. Reaching a
    limit in some other thread, including threads created by the task,
    will not produce an [Error]. *)


(** {1:initialisation Initialisation} *)

val start_memprof_limits : unit -> unit
(** Turn on the memprof-limits engine until the program terminates,
    which should be done once. Simply doing so should have no impact
    on performance. All the functions below producing a [result] raise
    {!Stdlib.Failure} if the engine is not started.

    Raises {!Stdlib.Failure} if the Memprof engine is already started. *)


(** {1:global_memory_limits Global memory limits} *)

val limit_global_memory : (unit -> 'a) -> 'a result
(** Executes the task given by the argument under the threat of being
    interrupted if the memory use of the program exceeds the limit set
    through {!set_global_memory_limit}. The memory use is given by the
    total memory allocated for the major heap.

    If interrupted, the program is likely in a state where further
    checks of memory limits will result in further interrupts being
    raised in concurrent and subsequent tasks running under a global
    memory limit. You can either let other tasks be interrupted too
    until the situation is resolved by the program shutting down
    gracefully, or you can call [Gc.compact ()] to attempt to free up
    space. The chance for a task to be interrupted relative to others
    is proportional to its allocation rate. *)

val set_global_memory_limit : int -> unit
(** Set the limit in kiB for the major heap word count over which one
    or more tasks executed with {!limit_global_memory} will be
    interrupted. A negative value disables the limit.

    Default: [-1]. *)


(** {1:allocation_limits Allocation limits} *)

val limit_allocations : limit:Int64.t -> (unit -> 'a) -> ('a * Int64.t) result
(** Executes the task given by the second argument under the threat of
    being interrupted if it allocates more than the limit. It counts
    allocations but not deallocations; as such it is a measure of work
    done. This measure is more portable and can be more suitable than
    time elapsed. The [~limit] is expressed in thousands of words (kw)
    allocated.

    Upon successful completion, the result is made of a pair of the
    return value of task, and the statistical approximation of the
    allocation count, expressed in kw allocated. The latter can be
    used to calibrate the chosen [~limit]. Refer to the documentation
    for the accuracy of the counting method and for advice about good
    margin for [~limit].

    If two calls to [limit_allocations] are nested, then allocations
    are accounted for both limits. In other words, a call to
    [limit_allocations] cannot be used to go beyond a limit already
    set in the current thread. *)

val max_allocation_limit : Int64.t
(** The maximal value for [~limit], which is equal to
    9'223'372'036'854'775 kw. *)


(** {1:token_limits Token limits} *)

module Token = Token

val limit_with_token : token:Token.t -> (unit -> 'a) -> 'a result
(** Executes the task given by the second argument under the threat of
    being interrupted if the {!Token} given as an argument is set by
    anyone.

    This can be used to interrupt an allocating task at a distance,
    from a monitor thread or from a signal handler, in a way which is
    safe for memprof-limits and for resources defined using
    {!Masking.with_resource}. Note that a task that does not allocate
    (for instance blocking on IO) will not be interrupted. *)


(** {1:rm Resource management} *)

module Masking = Masking
module Resource_bind = Resource_bind

val is_interrupted : unit -> bool
(** Returns [true] if the current thread is being interrupted. That
    is, this returns [true] between the moment an interrupt is raised
    and the moment the corresponding task terminates, even if if the
    interrupt is discarded by the task (in which case it is likely to
    arise again soon). Returns [false] otherwise.

    This has two purposes:
    - inside the [when] clause of a catch-all exception handler, to
      avoid catching and discarding an interrupt,
    - during resource release, to implement state poisoning or
      transaction rollback upon failure, so as to ensure that
      inconsistent state does not leak from the interrupted task. *)

(** {1:prof Profiling} *)

module Memprof = Memprof
