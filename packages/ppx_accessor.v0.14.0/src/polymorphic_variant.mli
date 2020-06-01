open! Base
open! Import

type t

val of_core_type : core_type -> t
val to_strs : t -> loc:location -> structure_item list
