type t =
  | Int of int (* int32? *)
  | Char of int
  | String of string
  | Unary of (t -> t)
  | Binary of (t -> t -> t)

val number_values: string -> t list
val int_plus: t
val int_minus: t
val int_times: t
val int_negate: t
val string_concat: t
val apply: t -> t -> t

val is_equal: t -> t -> bool
(** [is_equal a b] checks if [a] and [b] are the same value. *)

val compare: t -> t -> int
(** compare for sorting purposes. *)
