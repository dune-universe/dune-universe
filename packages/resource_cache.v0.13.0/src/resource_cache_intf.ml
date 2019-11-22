open! Core_kernel
open! Async_kernel
open! Import

module type S = sig
  type key
  type common_args
  type resource

  module Status : Status.S with type Key.t = key

  type t

  val init : config:Config.t -> log_error:(string -> unit) -> common_args -> t
  val status : t -> Status.t
  val config : t -> Config.t

  (** [with_ t key ~f] calls [f resource] where [resource] is either:

      1) An existing cached resource that was opened with key' such that
      [R.Key.equal key key']
      2) A newly opened resource created by [R.open_ key common_args], respecting
      the limits of [t.config]

      Returns an error if:
      - the cache is closed
      - [R.open_] returned an error
      - no resource is obtained before [give_up] is determined

      If [f] raises, the exception is not caught, but the [resource] will be
      closed and the [Cache] will remain in a working state (no resources are lost). *)
  val with_
    :  ?open_timeout:Time_ns.Span.t (** default [None] *)
    -> ?give_up:unit Deferred.t (** default [Deferred.never] *)
    -> t
    -> key
    -> f:(resource -> 'a Deferred.t)
    -> 'a Deferred.Or_error.t

  (** Like [with_] but classify the different errors *)
  val with_'
    :  ?open_timeout:Time_ns.Span.t
    -> ?give_up:unit Deferred.t
    -> t
    -> key
    -> f:(resource -> 'a Deferred.t)
    -> [ `Ok of 'a
       | `Gave_up_waiting_for_resource
       | `Error_opening_resource of Error.t
       | `Cache_is_closed
       ]
         Deferred.t


  (** Like [with_] and [with_'] except [f] is run on the first matching available resource
      (or the first resource that has availability to be opened).

      Preference is given towards resources earlier in the list, unless
      [~load_balance:true] has been specified, in which case preference is given to ensure
      that load is approximately balanced. The key with the least number of open
      connections will be favored. *)
  val with_any
    :  ?open_timeout:Time_ns.Span.t
    -> ?give_up:unit Deferred.t
    -> ?load_balance:bool
    -> t
    -> key list
    -> f:(resource -> 'a Deferred.t)
    -> (key * 'a) Deferred.Or_error.t

  val with_any'
    :  ?open_timeout:Time_ns.Span.t
    -> ?give_up:unit Deferred.t
    -> ?load_balance:bool
    -> t
    -> key list
    -> f:(resource -> 'a Deferred.t)
    -> [ `Ok of key * 'a
       | `Error_opening_resource of key * Error.t
       | `Gave_up_waiting_for_resource
       | `Cache_is_closed
       ]
         Deferred.t

  (** Tries [with_any'] in a loop (removing args that have open errors) until receiving an
      [`Ok], or until it has failed to open all resources in [args_list]. *)
  val with_any_loop
    :  ?open_timeout:Time_ns.Span.t
    -> ?give_up:unit Deferred.t
    -> ?load_balance:bool
    -> t
    -> key list
    -> f:(resource -> 'a Deferred.t)
    -> [ `Ok of key * 'a
       | `Error_opening_all_resources of (key * Error.t) list
       | `Gave_up_waiting_for_resource
       | `Cache_is_closed
       ]
         Deferred.t

  val close_started : t -> bool
  val close_finished : t -> unit Deferred.t

  (** Close all currently open resources and prevent the creation of new ones. All
      subsequent calls to [with_] and [immediate] fail with [`Cache_is_closed]. Any jobs
      that are waiting for a connection will return with [`Cache_is_closed]. The
      returned [Deferred.t] is determined when all jobs have finished running and all
      resources have been closed. *)
  val close_and_flush : t -> unit Deferred.t

end

module type Resource_cache = sig
  module type S = S


  (** [Cache.Make] creates a cache module that exposes a simple [with_] interface over its
      resources. The cache has the following properties:

      Resource reuse: When a resource [r] is opened, it will remain open until one of the
      following:
      - [f r] raised an exception where [f] was a function passed to [with_]
      - [r] has been idle for [idle_cleanup_after]
      - [r] has been used [max_resource_reuse] times
      - [close_and_flush] has been called on the cache

      When a resource is closed, either because of one of the above conditions, or because
      it was closed by other means, it no longer counts towards the limits.

      Limits: The cache respects the following limits:
      - No more than [max_resources] are open simultaneously
      - No more than [max_resources_per_id] are open simultaneously for a given id (args)
  *)
  module Make (R : Resource.S) () :
    S
    with type key := R.Key.t
     and type common_args := R.Common_args.t
     and type resource := R.t

  (** Wrap a resource that does not natively support a [has_close_started] operation
      in a simple record to add such tracking. *)
  module Make_simple (R : Resource.Simple) () :
    S
    with type key := R.Key.t
     and type common_args := R.Common_args.t
     and type resource := R.t

  (** Make a cache from a resource where the type clients wish to operate on is
      derived from, but not necessarily equal to, the type held by the cache. *)
  module Make_wrapped (R : Resource.S_wrapped) () :
    S
    with type key := R.Key.t
     and type common_args := R.Common_args.t
     and type resource := R.resource
end
