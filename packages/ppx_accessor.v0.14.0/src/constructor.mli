open! Base
open! Import

type t

val of_constructor_declaration : constructor_declaration -> t
val wildcard_patterns : t list -> loc:location -> pattern option
val to_strs : t -> wildcard:pattern option -> type_name:label -> structure_item list
