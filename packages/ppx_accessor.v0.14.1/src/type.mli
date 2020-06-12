open! Base
open! Import

type t

val of_type_declaration : type_declaration -> t
val to_strs : t -> structure_item list
