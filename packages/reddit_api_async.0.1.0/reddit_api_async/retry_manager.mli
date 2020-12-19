(** [Retry_manager] handles transient errors due to blips in networking or
 *  Reddit's infrastructure. *)

open! Core
open! Async
open Reddit_api_kernel

type t

val create : Connection.t -> t

(** [call t f] immediately calls [f] unless the last result of such a call
 *  shows a loss of service. In the latter case, all calls block, and [call]
 *  periodically calls a non-modifying API endpoint until service is restored.
 *
 *  "Loss of service" means that a [Cohttp] function raises or Reddit returns
 *  an HTTP server error status code. *)
val call : t -> 'a Api.t -> ('a, Cohttp.Response.t * Cohttp.Body.t) Deferred.Result.t

(** [yield_until_reddit_available] returns immediately if there is no known
 *  loss of service; it never causes an HTTP request. *)
val yield_until_reddit_available : t -> unit Deferred.t
