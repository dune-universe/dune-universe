(** Simple combinatorics. *)

(** [enumerate_subsets n l] enumerates all subsets of size [n] of [l]. *)
val enumerate_subsets : int -> 'a list -> 'a list list

(** [sample_without_replacement n l] samples a subset of size [n] from [l] without replacement.

    @raise Invalid_argument if [n > List.length l]
  *)
val sample_without_replacement :
  int -> 'a list -> ('a list * 'a list) Stats_intf.gen
