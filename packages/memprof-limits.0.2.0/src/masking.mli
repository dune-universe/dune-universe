(** Manage the masking of {i interrupts} (asynchronous exceptions)
    arising from memprof callbacks.

    See {{!recovering} the guide on recovering from interrupts raised
    via memprof-limits}.*)

val with_resource :
  acquire:('a -> 'b) -> 'a -> scope:('b -> 'c) -> release:('b -> unit) -> 'c
(** A combinator to define interrupt-safe resources (provided
    {!Memprof_limits} or {!Memprof_limits.Memprof} is used).

    [with_resource ~acquire x ~scope ~release] invokes [acquire x],
    then invokes [scope] on the resulting value, and then invokes
    [release] on the value, whether [scope] returns or raises an
    exception. The result of [scope] is then produced, whether it is
    a value or an exception.

    It is guaranteed that [release] is called upon return of
    [with_resource] on the result of [acquire] if and only if the
    latter returned normally. During the execution of acquire and
    release, no memprof callback is executed.

    The purpose of [with_resource] is to offer guarantees about the
    release of system or custom resources. It can therefore be used
    to ensure consistency of state, even in the event of
    interruption from memprof-limits. To achieve this, it is
    sufficient to fulfil the following conditions:
    + {i Strong exception-safety} for acquisition: the acquisition
      either succeeds, or if it fails, it does so by raising an
      exception, without having acquired the resource, for instance
      by undoing changes;
    + The release never fails.

    [with_resource] makes it possible to fulfil these conditions by
    preventing memprof-limits from interrupting the acquisition or
    the release.

    It does nothing to prevent other kinds of asynchronous exceptions
    (e.g. {!Stdlib.Sys.Break} raised from a signal handler) from
    arising, which could still leave the program in an inconsistent
    state. Also, it is not guaranteed that future versions will not
    delay these other kinds of asynchronous exceptions from arising,
    so this particular behaviour should not be relied upon. *)

val is_blocked : unit -> bool
(** Whether interrupts are currently blocked by {!with_resource}. *)

val assert_blocked : unit -> unit
(** Asserts [is_blocked ()]. *)
