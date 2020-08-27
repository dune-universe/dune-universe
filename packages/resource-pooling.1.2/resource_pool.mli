(** External resource pools.

    This module provides an abstraction for managing collections of resources.
    One example use case is for managing a pool of database connections, where
    instead of establishing a new connection each time you need one (which is
    expensive), you can keep a pool of opened connections and reuse ones that
    are free.

    It also provides the capability of:
    - specifying the maximum number of resources that the pool can manage
      simultaneously,
    - checking whether a resource is still valid before/after use, and
    - performing cleanup logic before dropping a resource.

    The following example illustrates how it is used with an imaginary
    [Db] module:

    {[
let uri = "postgresql://localhost:5432"

(* Create a database connection pool with max size of 10. *)
let pool =
  Resource_pool.create 10
    ~dispose:(fun connection -> Db.close connection |> Lwt.return)
    (fun () -> Db.connect uri |> Lwt.return)

(* Use the pool in queries. *)
let create_user name =
  Resource_pool.use pool (fun connection ->
      connection
      |> Db.insert "users" [("name", name)]
      |> Lwt.return
    )
]}

    Note that this is {e not} intended to keep a pool of system threads.
    If you want to have such pool, consider using {!Lwt_preemptive}. *)

type 'a t
  (** A pool containing elements of type ['a]. *)

val create :
  ?validate : ('a -> bool Lwt.t) ->
  ?check : (exn -> 'a -> bool Lwt.t) ->
  ?dispose : ('a -> unit Lwt.t) ->
  int ->
  (unit -> 'a Lwt.t) -> 'a t
  (** [create n ?check ?validate ?dispose f] creates a new pool with at most
      [n] elements. [f] is used to create a new pool element.  Elements are
      created on demand and re-used until disposed of. [f] may raise the
      exception [Resource_invalid] to signal a failed resource creation. In this
      case [use] will re-attempt to create the resource (according to
      [creation_attempts]).

      @param validate is called each time a pool element is accessed by {!use},
      before the element is provided to {!use}'s callback.  If
      [validate element] resolves to [true] the element is considered valid and
      is passed to the callback for use as-is.  If [validate element] resolves
      to [false] the tested pool element is passed to [dispose] then dropped,
      with a new one is created to take [element]'s place in the pool.

      @param check is called after the resolution of {!use}'s callback when the
      resolution is a failed promise.  [check element is_ok] must call [is_ok]
      exactly once with [true] if [element] is still valid and [false]
      otherwise.  If [check] calls [is_ok false] then [dispose] will be run
      on [element] and the element will not be returned to the pool.

      @param dispose is used as described above and by {!clear} to dispose of
      all elements in a pool.  [dispose] is {b not} guaranteed to be called on
      the elements in a pool when the pool is garbage collected.  {!clear}
      should be used if the elements of the pool need to be explicitly disposed
      of. *)

(** set the maximum size of the pool *)
val set_max : 'a t -> int -> unit

(** exception to be thrown by the function supplied to [use] when a resource
    is no longer valid and therefore to be disposed of;
    [safe] determines whether it is safe to relaunch the function supplied to
    [use]. Typically this refers to whether side-effects (that should not occur
    a second time) have been effectuated before the exception was raised. *)
exception Resource_invalid of {safe : bool}

val use :
  ?creation_attempts:int ->
  ?usage_attempts:int ->
  'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  (** [use p f] requests one free element of the pool [p] and gives it to
      the function [f]. The element is put back into the pool after the
      promise created by [f] completes.

      In case the resource supplied to [f] is no longer valid, [f] can throw a
      [Resource_invalid] exception in which case the resource is disposed of.

      The parameter [creation_attempts] (default: [1]) controls the number of
      resource creation attempts that are made in case the creation function
      raises the [Resource_invalid] exception.

      The parameter [usage_attempts] (default: [1]) controls the number of
      attempts that are made in case [f] raises the [Resource_invalid]
      exception with [safe] set to true. After each attempt the resource is
      disposed of.

      In the case that [p] is exhausted and the maximum number of elements
      is reached, [use] will wait until one becomes free. *)

val clear : 'a t -> unit Lwt.t
  (** [clear p] will clear all elements in [p], calling the [dispose] function
      associated with [p] on each of the cleared elements.  Any elements from
      [p] which are currently in use will be disposed of once they are
      released.

      The next call to [use p] after [clear p] guarantees a freshly created
      pool element.

      Disposals are performed sequentially in an undefined order. *)

exception Resource_limit_exceeded

val add : ?omit_max_check:bool -> 'a t -> 'a -> unit
  (** By [add p c] you can add an existing resource element [c] to pool [p].
      This function may raise a [Resource_limit_exceeded] exception. If
      [omit_max_check] is [true] (default: [false]), then this exception will
      not be raised. Instead the maximum number of resources might be exceeded
      and more than [p.max] elements will be available to the user. *)

val wait_queue_length : _ t -> int
  (** [wait_queue_length p] returns the number of {!use} requests currently
      waiting for an element of the pool [p] to become available. *)

val wait_queue_delay : _ t -> float
  (** [wait_queue_length p] returns the age of the oldest {!use} request
      currently waiting for an element of the pool [p] to become available. *)
