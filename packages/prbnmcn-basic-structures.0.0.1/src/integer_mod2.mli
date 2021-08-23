(** The finite field of integers modulo 2 *)

(** The type of integers modulo 2 *)
type t = Zero | One

include Basic_intf.Field_std with type t := t
