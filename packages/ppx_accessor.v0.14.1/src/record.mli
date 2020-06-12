open! Base
open! Import

type t

val of_label_declarations : label_declaration list -> t
val to_strs : t -> type_name:label -> structure_item list

module Inline : sig
  val to_strs
    :  t
    -> type_name:label
    -> constructor_name:Name.t
    -> wildcard:pattern option
    -> loc:location
    -> structure_item list
end
