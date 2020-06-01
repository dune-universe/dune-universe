open! Core_kernel
open! Import
include module type of Accessor.List

(** Accesses the prefixes of the lists whose length is the same as whichever list is
    shorter. *)
val zipped : (_, ('a * 'b) list, 'a list * 'b list, [< field ]) Accessor.Simple.t
