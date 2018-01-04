open! Core
open Async_kernel

module Limiter : sig
  type t
  val create : rate_per_sec:Byte_units.t -> t
end

(** Use sendfile(2) to deliver all bytes from [file] to [socket_fd].
    Return success iff the whole file was successfully delivered. *)
val sendfile
  :  ?limiter:Limiter.t           (** default: No limiter *)
  -> ?delivery_unit:Byte_units.t  (** default: 2 Megabytes *)
  -> socket_fd:Async_unix.Fd.t
  -> file:string
  -> unit
  -> unit Deferred.Or_error.t
