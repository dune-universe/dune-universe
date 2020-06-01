open! Core
module Resource : T

module Test_cache : sig
  include
    Resource_cache.S
    with type key := int
     and type common_args := unit
     and type resource := Resource.t

  val init : config:Resource_cache.Config.t -> unit -> t
end
