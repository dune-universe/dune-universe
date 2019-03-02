module Stable = struct
  open! Core_kernel.Core_kernel_stable

  module V1 = struct
    type t =
      { max_open_connections : int
      ; cleanup_idle_connection_after : Time_ns.Span.V2.t
      ; max_connections_per_address : int
      ; max_connection_reuse : int
      }
    [@@deriving sexp, bin_io]
  end
end

open! Core_kernel
open! Async_kernel
open! Import

type t = Stable.V1.t =
  { max_open_connections : int
  ; cleanup_idle_connection_after : Time_ns.Span.t
  ; max_connections_per_address : int
  ; max_connection_reuse : int
  }
[@@deriving sexp_of, fields, compare]

let create = Fields.create

let default =
  { max_open_connections = 500
  ; cleanup_idle_connection_after = Time_ns.Span.of_sec 5.
  ; max_connections_per_address = 10
  ; max_connection_reuse = 10
  }
;;

let to_cache_config t =
  Config.create
    ~max_resources:t.max_open_connections
    ~idle_cleanup_after:t.cleanup_idle_connection_after
    ~max_resources_per_id:t.max_connections_per_address
    ~max_resource_reuse:t.max_connection_reuse
;;

let of_cache_config (cache_config : Config.t) =
  create
    ~max_open_connections:cache_config.max_resources
    ~cleanup_idle_connection_after:cache_config.idle_cleanup_after
    ~max_connections_per_address:cache_config.max_resources_per_id
    ~max_connection_reuse:cache_config.max_resource_reuse
;;
