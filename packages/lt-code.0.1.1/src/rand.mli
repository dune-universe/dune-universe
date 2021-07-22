type rng

type bounded_rng

val create_rng : int -> rng

val create_bounded_rng : bound:int -> int -> bounded_rng

val gen_int : rng -> int -> int

val gen_int_global : int -> int

val gen_int_bounded : bounded_rng -> int

val hash_int : int -> int
