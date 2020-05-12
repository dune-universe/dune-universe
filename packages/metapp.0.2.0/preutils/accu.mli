type 'a t

val empty : 'a t

val add : 'a -> 'a t -> int * 'a t

val length : 'a t -> int

val to_array : 'a t -> 'a array
