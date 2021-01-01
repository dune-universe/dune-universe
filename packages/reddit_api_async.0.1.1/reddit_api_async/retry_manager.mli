(** [Retry_manager] handles transient errors due to blips in networking or
    Reddit's infrastructure.

    {b Warning.} Do not use [Retry_manager] if it is critical that you do not
    perform the same action twice. Reddit has been known to send HTTP server
    error statuses even while successfully handling the request. Therefore,
    [Retry_manager] cannot guarantee that a request had no effect before
    retrying it - it just trusts Reddit when it says that there was an error. 
*)

open! Core
open! Async
open Reddit_api_kernel

type t

val create : Connection.t -> t

(** [call t f] immediately calls [f] unless the last result of such a call
    shows a loss of service. In the latter case, all calls block, and [call]
    periodically calls a read-only API endpoint until service is restored.
  
    "Loss of service" means that a [Cohttp] function raises or Reddit returns
    an HTTP server error status code.
*)
val call : t -> 'a Api.t -> ('a, Cohttp.Response.t * Cohttp.Body.t) Deferred.Result.t

(** [yield_until_reddit_available] returns immediately if there is no known
    loss of service; it never causes an HTTP request. *)
val yield_until_reddit_available : t -> unit Deferred.t
