open! Core
open! Async

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type body_stream

val iter_body : body_stream -> f:(string -> unit Deferred.t) -> unit Deferred.t
val iter_body' : body_stream -> f:(string -> unit) -> unit Deferred.t

type service =
  Cohttp.Request.t * body_stream ->
  (Cohttp.Response.t * [ `Bigstring of bigstring | `String of string ])
  Deferred.t

type conn

val create : Fd.t -> read_buffer_size:int -> write_buffer_size:int -> conn
val run : conn -> service -> unit Deferred.t
