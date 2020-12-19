(** [Username] is a string identifier module that does some normalization:

   - Hashes and comparisons are caseless.
   - "u/" and "/u/" prefixes are dropped.
*)

open! Core_kernel
include Identifiable.S

val of_string_or_deleted : string -> t option
