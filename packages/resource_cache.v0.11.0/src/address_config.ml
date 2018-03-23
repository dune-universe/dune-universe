open! Core

type t =
  { max_open_connections          : int
  ; cleanup_idle_connection_after : Time.Span.t
  ; max_connections_per_address   : int
  ; max_connection_reuse          : int
  } [@@deriving sexp, fields, bin_io, compare]

let create = Fields.create

let to_cache_config t =
  Cache.Config.create
    ~max_resources:t.max_open_connections
    ~idle_cleanup_after:t.cleanup_idle_connection_after
    ~max_resources_per_id:t.max_connections_per_address
    ~max_resource_reuse:t.max_connection_reuse

let of_cache_config (cache_config : Cache.Config.t) =
  create
    ~max_open_connections:cache_config.max_resources
    ~cleanup_idle_connection_after:cache_config.idle_cleanup_after
    ~max_connections_per_address:cache_config.max_resources_per_id
    ~max_connection_reuse:cache_config.max_resource_reuse
