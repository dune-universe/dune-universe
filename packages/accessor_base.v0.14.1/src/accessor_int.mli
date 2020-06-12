open! Base
open! Import

(** The below functions all access modified versions of an [int]. *)

val negated : (_, int, int, [< isomorphism ]) Accessor.Simple.t
val added : int -> (_, int, int, [< isomorphism ]) Accessor.Simple.t
val subtracted : int -> (_, int, int, [< isomorphism ]) Accessor.Simple.t
val incremented : (_, int, int, [< isomorphism ]) Accessor.Simple.t
val decremented : (_, int, int, [< isomorphism ]) Accessor.Simple.t
val bit_negated : (_, int, int, [< isomorphism ]) Accessor.Simple.t
val bit_xored : int -> (_, int, int, [< isomorphism ]) Accessor.Simple.t

(** Access a specific bit in an [int]. [bit_at_exn i] raises if [i >= Int.num_bits] *)
val bit_at_exn : int -> (_, bool, int, [< field ]) Accessor.Simple.t
