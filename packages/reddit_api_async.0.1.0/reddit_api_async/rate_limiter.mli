open! Core
open! Async

type t [@@deriving sexp_of]

val by_headers : unit -> t
val with_minimum_delay : delay:Time_ns.Span.t -> t

val with_t
  :  t
  -> f:(unit -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
  -> time_source:Time_source.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
