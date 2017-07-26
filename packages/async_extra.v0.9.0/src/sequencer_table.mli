(** A table of sequencers indexed by key, so that at any moment for each key there is at
    most one job running.

    An ['a Sequencer_table.Make(Key).t] is similar in concept to:

    {[
      { mutable state : 'a option
      ; jobs  : 'a option Throttle.Sequencer.t
      } Key.Table.t
    ]}

    It allows one to run jobs that are indexed by a key, while allowing jobs with distinct
    keys to run simultaneously, and ensuring that for any given key, at most one job with
    that key is running at a time.  A sequencer table maintains optional state for each
    key, and supplies that state to each running job indexed by that key.

    The implementation of a sequencer table is optimized for having a large number of keys
    with state, while only a few keys with active jobs at any given time.  So, it is
    implemented with two tables, one of states and one of sequencers:

    {[
      { states : 'a Key.Table.t
      ; jobs   : ('a option -> unit Deferred.t) Sequencer.t Key.Table.t
      }
    ]}

    The implementation automatically adds a sequencer to the [jobs] table, if necessary,
    when one adds a job, and automatically removes a sequencer from the [jobs] table
    whenever the sequencer has no jobs to run.

    The implementation does not automatically release state; one must call [set_state t
    ~key None]. *)

open! Core
open! Import

module Make (Key : Hashable) : sig

  (** Every [Key.t] in the table has an associated [state], which each job running on that
      key gets access to.  Jobs maybe have an associated [job_tag] which is provided
      purely to assist debugging, as the tag is included in the sexp serialization of
      [t]. *)
  type ('state, 'job_tag) t [@@deriving sexp_of]

  val create : unit -> _ t

  (** [enqueue t ~key f] enqueues [f] for [key].  [f] will be called with the state of
      [key] when invoked.

      Invariant 1: it is guaranteed that [f] will not be called immediately.

      Invariant 2: if [f] raises, then the exception will be raised to the monitor in
      effect when [enqueue] was called.  Subsequent jobs for [key] will proceed.

      Invariant 3: to avoid race, there are no deferred operations between finding the
      state and calling [f] with the state found.  Otherwise, the user would need to
      consider the race that the state passed to [f] might have been changed by
      [set_state]. *)
  val enqueue
    :  ('state, 'job_tag) t
    -> key:Key.t
    -> ?tag:'job_tag
    -> ('state option -> 'b Deferred.t)
    -> 'b Deferred.t

  (** [set_state t key state_opt] sets the state for [key] immediately.  The state will be
      kept internally until set to [None] *)
  val set_state : ('state, _) t -> key:Key.t -> 'state option -> unit

  val find_state : ('state, _) t -> Key.t -> 'state option

  (** [num_unfinished_jobs t key] returns the number of jobs for [key] including including
      pending and running. *)
  val num_unfinished_jobs : _ t -> Key.t -> int

  (** [mem t key] returns [true] if there is state or an pending/running job *)
  val mem : _ t -> Key.t -> bool

  (** Fold over keys with states or pending/running jobs. It's safe to mutate ([enqueue]
      or [set_state]) when folding *)
  val fold : ('state, _) t -> init:'b -> f:('b -> key:Key.t -> 'state option -> 'b) -> 'b

  (** The result is determined when all jobs enqueued before this are finished.  The
      implementation adds a new job to every key currently with at least one running job
      attached, so it will affect [num_unfinished_jobs] *)
  val prior_jobs_done : (_, _) t -> unit Deferred.t

end
