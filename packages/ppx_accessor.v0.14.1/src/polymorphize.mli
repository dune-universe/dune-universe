open! Base
open! Import

val polymorphize : loc:location -> expr:expression -> expression
val binding : loc:location -> name:label -> expr:expression -> structure_item
