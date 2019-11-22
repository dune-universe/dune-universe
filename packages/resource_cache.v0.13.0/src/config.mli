open! Core_kernel
open! Async_kernel
open! Import

type t =
  { max_resources : int
  ; idle_cleanup_after : Time_ns.Span.t
  ; max_resources_per_id : int
  ; max_resource_reuse : int
  ; close_idle_resources_when_at_limit : bool
  (** The cache will close the least recently used idle resource when there is pressure at
      the [max_resources] limit. *)
  }
[@@deriving compare, fields, sexp_of]

val create
  :  max_resources:int
  -> idle_cleanup_after:Time_ns.Span.t
  -> max_resources_per_id:int
  -> max_resource_reuse:int
  -> close_idle_resources_when_at_limit:bool
  -> t

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end

  module V1 : sig
    type t =
      { max_resources : int
      ; idle_cleanup_after : Time_ns.Span.t
      ; max_resources_per_id : int
      ; max_resource_reuse : int
      }
    [@@deriving bin_io, sexp]

    val to_v2 : t -> V2.t
  end
end
