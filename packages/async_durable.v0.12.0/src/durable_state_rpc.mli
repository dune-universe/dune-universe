open Core_kernel
open Async_kernel
open Async_rpc_kernel

(** This module is designed to help processess maintain state RPCs that will automatically
    recover from lost connections. It exposes to the client when the subscription has been
    lost or recovered, so the client can choose how to handle a lost connection. *)

module Update : sig
  type ('state, 'update, 'error, 'metadata) t =
    | Attempting_new_connection
    | Connection_success of 'metadata
    | Lost_connection
    | Failed_to_connect of Error.t
    | Rpc_error of 'error
    | Update of 'update
    | State of 'state
end

(** [create] will immediately dispatch the the supplied [Rpc.State_rpc.t] with [query]
    over the [Rpc.Connection.t Async_durable.t]. If a connection attempt fails or if the
    subscription closes, it waits [resubscribe_delay] and dispatches again to create a new
    subscription. The pipe returned by [create] contains all the responses that come over
    the internal subscription, as well as updates about the state of the [t].

    Closing the returned pipe will permanently close the subscription.

    It is guaranteed that every [Connection_success] message will be immediately followed
    by a [State] message.
*)
val create
  :  Rpc.Connection.t Durable.t
  -> ('query, 'state, 'update, 'error) Rpc.State_rpc.t
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> ('state, 'update, 'error, Rpc.State_rpc.Metadata.t) Update.t Pipe.Reader.t

(** [create_or_fail] will return an [Error e] if the initial attempt to dispatch the
    supplied [Rpc.Pipe_rpc.t] does not succeed, or an [Ok (Error 'error)] if the initial
    dispatch returns a server side rpc error. *)
val create_or_fail
  :  Rpc.Connection.t Durable.t
  -> ('query, 'state, 'update, 'error) Rpc.State_rpc.t
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> (('state, 'update, 'error, Rpc.State_rpc.Metadata.t) Update.t Pipe.Reader.t, 'error)
       Result.t Or_error.t Deferred.t

(** [create_versioned], [create_or_fail_versioned], [create_versioned'],
    [create_or_fail_versioned'] are identical to [create] and [create_or_fail] but work
    for [Caller_converts] and [Both_converts] Versioned State RPCs.
*)

val create_versioned
  :  Versioned_rpc.Connection_with_menu.t Durable.t
  -> (module Versioned_rpc.Both_convert.State_rpc.S
       with type caller_query  = 'query
        and type caller_state  = 'state
        and type caller_update = 'update
        and type caller_error  = 'error)
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> ('state, 'update Or_error.t, 'error, Rpc.State_rpc.Metadata.t) Update.t Pipe.Reader.t

val create_versioned'
  :  Versioned_rpc.Connection_with_menu.t Durable.t
  -> (module Versioned_rpc.Caller_converts.State_rpc.S
       with type query  = 'query
        and type state  = 'state
        and type update = 'update
        and type error  = 'error)
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> ('state, 'update Or_error.t, 'error, Rpc.State_rpc.Metadata.t) Update.t Pipe.Reader.t

val create_or_fail_versioned
  :  Versioned_rpc.Connection_with_menu.t Durable.t
  -> (module Versioned_rpc.Both_convert.State_rpc.S
       with type caller_query  = 'query
        and type caller_state  = 'state
        and type caller_update = 'update
        and type caller_error  = 'error)
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> (('state, 'update Or_error.t, 'error, Rpc.State_rpc.Metadata.t)
        Update.t Pipe.Reader.t
     , 'error) Result.t Or_error.t Deferred.t

val create_or_fail_versioned'
  :  Versioned_rpc.Connection_with_menu.t Durable.t
  -> (module Versioned_rpc.Caller_converts.State_rpc.S
       with type query  = 'query
        and type state  = 'state
        and type update = 'update
        and type error  = 'error)
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> (('state, 'update Or_error.t, 'error, Rpc.State_rpc.Metadata.t)
        Update.t Pipe.Reader.t
     , 'error) Result.t Or_error.t Deferred.t

(** [Expert] is only used to build [Durable_pipe_rpc] off the same implementation as
    [Durable_state_rpc]. If other similar [Rpc]s come into being, they can also take
    advantage.
*)
module Expert : sig
  val create
    :  'connection Durable.t
    -> dispatch:('connection -> ('state * 'update Pipe.Reader.t * 'metadata, 'error)
                                  Result.t Or_error.t Deferred.t)
    -> resubscribe_delay:Time.Span.t
    -> ('state, 'update, 'error, 'metadata) Update.t Pipe.Reader.t

  val create_or_fail
    :  'connection Durable.t
    -> dispatch:('connection -> ('state * 'update Pipe.Reader.t * 'metadata, 'error)
                                  Result.t Or_error.t Deferred.t)
    -> resubscribe_delay:Time.Span.t
    -> (('state, 'update, 'error, 'metadata) Update.t Pipe.Reader.t, 'error)
         Result.t Or_error.t Deferred.t
end
