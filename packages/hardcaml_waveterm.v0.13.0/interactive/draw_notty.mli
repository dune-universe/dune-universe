open! Import

(** Notty based gfx API. We need to use an intermediate in memory representation for
    notty.
*)
include
  module type of Draw.In_memory with type style = Draw.In_memory.style

val to_image : ctx -> Notty.I.t
