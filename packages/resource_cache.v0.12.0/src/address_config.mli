open! Core_kernel
open! Import

type t =
  { max_open_connections : int
  ; cleanup_idle_connection_after : Time_ns.Span.t
  ; max_connections_per_address : int
  ; max_connection_reuse : int
  }
[@@deriving sexp_of, fields, compare]

val create
  :  max_open_connections:int
  -> cleanup_idle_connection_after:Time_ns.Span.t
  -> max_connections_per_address:int
  -> max_connection_reuse:int
  -> t

val default : t
val to_cache_config : t -> Config.t
val of_cache_config : Config.t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
  end
end
