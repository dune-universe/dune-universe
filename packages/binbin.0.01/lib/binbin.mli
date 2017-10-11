type t

val init : int -> t

val size : t -> int

val take : int -> t -> t

val make : int -> t -> t

val map : (t -> t) -> t -> t

val mapi : (int -> t -> t) -> t -> t

val dmap : (t -> t -> t) -> t -> t -> t

val foldr : (t -> 'a -> 'a) -> t -> 'a -> 'a

val foldl : ('a -> t -> 'a) -> 'a -> t -> 'a

val flip : t -> t

val flip_bit_at : int -> t -> t

val msbit : t -> t

val lsbit : t -> t

val pad_left : int -> t -> t

val pad_right : int -> t -> t

val concat : t -> t -> t

val reverse : t -> t

val normalize : t -> t -> t * t

val irreducible : t -> t

val b_xor : t -> t -> t

val b_or : t -> t -> t

val b_and : t -> t -> t

val b_not : t -> t

val find_first_one : t -> t

val count_leading_zeros : t -> t

val count_trailing_zeros : t -> t

val cardinality : t -> t

val hamming_distance : t -> t -> t

val of_int : int -> t

val of_char : char -> t

val of_string : string -> t

val to_int : t -> int

val to_char : t -> char

val to_ascii : t -> string

val unsafe_s : t -> string

val unsafe_b : string -> t

(**
 * TODO:
     * implement add and sub and mult
     * implement shift bits
     * clean up api and re-org
     * push on github
     * think about float support
     *)
