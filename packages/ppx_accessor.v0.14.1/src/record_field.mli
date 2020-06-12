open! Base
open! Import

type t

val of_label_declaration : label_declaration -> t
val to_str : t -> type_name:label -> structure_item
val to_str_singleton : t -> type_name:label -> structure_item

module Inline : sig
  val to_str
    :  t
    -> type_name:label
    -> constructor_name:Name.t
    -> wildcard:pattern option
    -> structure_item

  val to_str_singleton
    :  t
    -> type_name:label
    -> constructor_name:Name.t
    -> wildcard:pattern option
    -> structure_item
end
