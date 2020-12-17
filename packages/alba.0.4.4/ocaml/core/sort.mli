type t =
  | Proposition
  | Any of int


val compare: t -> t -> int
(** compare for sorting purposes. *)

(** [is_sub s1 s2] Is [s1] a subtype of [s2] (or equal)? *)
val is_sub: t -> t -> bool

(** [is_super s1 s2] Is [s1] a supertype of [s2] (or equal)? *)
val is_super: t -> t -> bool

val type_of: t -> t

val pi_sort: t -> t -> t
(**
   [pi_sort sA sB] computes the sort of a product term

   {[
       all (x: A): B
   ]}

   where [A] has sort [sA] and [B] has sort [sB].
 *)
