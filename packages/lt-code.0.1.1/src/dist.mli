type t

val robust_soliton_dist : k:int -> t

val choose_n : t -> int -> int array

val choose_into : ?offset:int -> t -> int array -> unit
