(** Pipe_iterator provides a way of walking through the elements of a pipe or other data
    stream that can only be computed once, but simulating multiple pointers into that
    sequence such that each of the pointers can be advanced separately. It does so without
    storing more of the sequence in memory than necessary.

    The primary function that enables this is the [copy] function, below. *)

open! Core
open! Async

type 'a t

(** Create an iterator from a function [f] that will be repeatedly called to get
    the next element of the sequence, and a function [close] that will be called
    once at the end when no more elements are left. *)
val create: f:(unit -> [`Ok of 'a | `Eof] Deferred.t) -> close:(unit -> unit) -> 'a t

(** Create an iterator from a pipe, and close it and all copies when [f] is finished.
    This consumes the pipe - the pipe should NOT be used for anything else after this. *)
val with_pipe: 'a Pipe.Reader.t -> f:('a t -> 'b Deferred.t) -> 'b Deferred.t

(** Same as [with_pipe], but without the automatic close. The user is responsible for
    calling [close] on the iterator and all copies. *)
val of_pipe: 'a Pipe.Reader.t -> 'a t

(** Copied iterators share the same underlying instance of the sequence. All copies of an
    iterator can be advanced independently while still only computing the underlying
    data stream once.

    In general, the portion of the sequence between the leftmost non-closed iterator and
    the rightmost point in the sequence reached by any iterator (even if later closed)
    will be kept in memory. Users should avoid having widely diverent iterators if memory
    usage is a concern. *)
val copy: 'a t -> 'a t


(** Only close the iterator on which this is called, and does NOT affect copies,
    which must be separately closed. The underlying pipe or other data source is closed
    once all iterators are closed OR at least one iterator has reached the end.
    Calling close more than once is safe.
    Calling any function other than close on a closed iterator will raise an exception. *)
val close: _ t -> unit

(** Same, but affects all copies or iterators this was copied from. *)
val close_everything: _ t -> unit

val read: 'a t -> [ `Eof | `Ok of 'a ] Deferred.t
val peek: 'a t -> [ `Eof | `Ok of 'a ] Deferred.t

(** The following are the same, except that they return immediately and report
    [`Nothing_available] if no iterator has yet been advanced past this point in the
    sequence via [read] or [peek].  *)
val read_now: 'a t -> [ `Eof | `Ok of 'a | `Nothing_available ]
val peek_now: 'a t -> [ `Eof | `Ok of 'a | `Nothing_available ]
