
open Parsetree

val derive : core_type -> core_type -> expression list
(** Given an input type [itype] and an output type [otype], returns a
   (possibly empty) list of expressions of type [itype -> otype]. *)
