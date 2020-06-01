open! Base
open! Import

type t

val of_constructor_declarations : constructor_declaration list -> t
val to_strs : t -> loc:location -> type_name:label -> structure_item list
