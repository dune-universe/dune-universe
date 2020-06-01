open! Base
open! Import

val gen_symbol : label -> loc:location -> pattern * expression
val map_with_context : 'a list -> f:('a -> context:'a list -> 'b) -> 'b list
val unsupported : loc:location -> label -> _
