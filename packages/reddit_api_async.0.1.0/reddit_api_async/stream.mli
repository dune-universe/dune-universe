open! Core
open! Async
open Reddit_api_kernel

val iter
  :  (module Hashable.S with type t = 'id)
  -> Connection.t
  -> get_listing:(before:'id option -> limit:int -> 'thing list Api.t)
  -> get_before_parameter:('thing -> 'id)
  -> f:('thing -> unit Deferred.t)
  -> _ Deferred.t

val fold
  :  (module Hashable.S with type t = 'id)
  -> Connection.t
  -> get_listing:(before:'id option -> limit:int -> 'thing list Api.t)
  -> get_before_parameter:('thing -> 'id)
  -> init:'state
  -> f:('state -> 'thing -> 'state Deferred.t)
  -> on_error:('state -> Api.Api_error.t -> 'state Deferred.t)
  -> _ Deferred.t

val fold_until_finished
  :  (module Hashable.S with type t = 'id)
  -> Connection.t
  -> get_listing:(before:'id option -> limit:int -> 'thing list Api.t)
  -> get_before_parameter:('thing -> 'id)
  -> init:'state
  -> f:('state -> 'thing -> ('state, 'result) Continue_or_stop.t Deferred.t)
  -> on_error:
       ('state -> Api.Api_error.t -> ('state, 'result) Continue_or_stop.t Deferred.t)
  -> 'result Deferred.t
