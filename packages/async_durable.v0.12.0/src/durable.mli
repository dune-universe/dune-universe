open Core_kernel
open Async_kernel

(** Durable is designed to help recover from simple errors when using mutable data types
    that silently fail and can be easily rebuilt or created anew. A clear candidate is
    Rpc.Connection.t.

    Durable and Persistent_connection have overlapping functionality when Durable is used
    for an Rpc.Connection.t. Consider using Durable if you want out-of-the-box support for
    Durable Pipe and State RPC connections, or if you prefer the with_ based interface,
    which returns connection level errors to the calling code and avoids the complexities
    that can arise from Deferreds that never become determined.
*)

type 'a t

(** [to_create] tells the Durable how to build a fresh 'a value.

    [is_broken] tests whether the current 'a value can be used. It should return true when
    you want the Durable to attempt to rebuild or recreate the 'a.

    [to_rebuild] Is called on the broken 'a. It should return a "fixed" 'a.

    When the function [with_] below is called, it uses [is_broken] to test if the current
    Durable value is broken. If not, it calls either [to_create] or [to_rebuild]. If
    [to_rebuild] is None (as in the default case), the Durable will try to create a fresh
    value with [to_create].

    [create] does not create the Durable value. The first call to [to_create] will be made
    on the first use of [with_].
*)
val create
  :  to_create:(unit -> 'a Deferred.Or_error.t)
  -> is_broken:('a -> bool)
  -> ?to_rebuild:('a -> 'a Deferred.Or_error.t)
  -> unit
  -> 'a t

(** [create_or_fail] immediately calls [to_create], returning an error if that first
    attempt fails. This function will also return an error if the initial durable value is
    broken.
*)
val create_or_fail
  :  to_create:(unit -> 'a Deferred.Or_error.t)
  -> is_broken:('a -> bool)
  -> ?to_rebuild:('a -> 'a Deferred.Or_error.t)
  -> unit
  -> 'a t Or_error.t Deferred.t

(** [with_] applies the given function to the Durable value if [is_broken] returns
    false. If [is_broken] return true, it first calls [to_create] or [to_rebuild].  This
    function will return an error if either [to_create] or [to_rebuild] returns an error,
    or if the rebuilt or recreated durable value is broken. [is_broken] is checked
    immediately before calling [f].

    We will only make one attempt to rebuild a broken value. If a call to [to_rebuild]
    returns [Error _], we will drop the previously built value. The next call to [with_]
    will call [to_create].

    [with_] will raise if [f] raises.
*)
val with_ : 'a t -> f:('a -> 'b Deferred.Or_error.t) -> 'b Deferred.Or_error.t
