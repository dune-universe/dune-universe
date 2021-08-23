(** The ring of integers *)

include Basic_intf.Ring_std with type t = Z.t

val div : t -> t -> t
