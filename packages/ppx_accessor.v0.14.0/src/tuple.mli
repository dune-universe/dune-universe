open! Base
open! Import

type t

val empty : t
val singleton : t
val of_core_types : core_type list -> t

module Inline : sig
  val wildcard_pattern : t -> loc:location -> pattern option

  val to_str
    :  t
    -> loc:location
    -> name:Name.t
    -> wildcard:pattern option
    -> structure_item
end

module Polymorphic_variant : module type of Inline
