(** [Connection] manages the connection to Reddit, including authentication and
    rate limiting behavior.

    It is responsible for taking the endpoint specifications in
    {!module:Reddit_api_kernel.Api} and actually performing HTTP requests.

    Consider wrapping your [Connection] in a {!module:Retry_manager} if you are
    writing a long-running process and want to just retry forever on transient
    errors.

    {1 Authentication }

    [Connection] currently only supports the "script" app type. See 
    {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types}
    Reddit's documentation on app types}.

    {1 Rate-limiting behavior }

    [Connection] enforces two different forms of rate-limiting:

    {2 HTTP rate-limiting headers }

    Reddit tracks API usage and requires that a client make no more than 600
    requests in a 10 minute period.

    Rather than simply keeping a counter of requests internally, [Connection]
    reads Reddit's API response headers to learn about the request quota,
    including the number of remaining requests and the time until the quota
    resets. This allows multiple [Connection.t]s with the same credentials to
    run in parallel, accounting for each others' quota usage without explicit
    coordination.

    {2 Minimum time between requests }

    In order to abide by /u/kemitche's 
    {{:https://www.reddit.com/r/redditdev/comments/1yxrp7/formal_ratelimiting_headers/} 
    request} to "be reasonable" and not slam all 600 requests in as quickly as
    possible, [Connection] also enforces a 100ms delay between requests.
*)

open! Core
open! Async
open Reddit_api_kernel

module Credentials : sig
  type t =
    { client_id : string
    ; client_secret : string
    ; password : string
    ; username : string
    }
  [@@deriving sexp]
end

type t [@@deriving sexp_of]

val create : Credentials.t -> user_agent:string -> t
val call : t -> 'a Api.t -> ('a, Api.Api_error.t) Result.t Deferred.t
val call_exn : t -> 'a Api.t -> 'a Deferred.t

(** [call_raw] returns the raw HTTP response from Reddit, or any exception that
    was raised by Cohttp. *)
val call_raw
  :  t
  -> 'a Api.t
  -> (Cohttp.Response.t * Cohttp.Body.t, Exn.t) Result.t Deferred.t

(** Any connection can be turned into an RPC server, acting as a shared
    connection for multiple client [Connection.t]s. Rate limiting is managed
    on the server side. *)
module Remote : sig
  val serve
    :  t
    -> where_to_listen:(([< Socket.Address.t ] as 'a), 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t

  val connect_exn : [< Socket.Address.t ] Tcp.Where_to_connect.t -> t Deferred.t
end

module For_testing : sig
  val with_cassette
    :  Filename.t
    -> credentials:Credentials.t
    -> f:(t -> 'a Deferred.t)
    -> 'a Deferred.t
end
