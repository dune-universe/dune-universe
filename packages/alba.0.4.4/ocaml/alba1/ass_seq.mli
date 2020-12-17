type 'a t

val empty: unit -> 'a t

val count_first: 'a t -> int

val count: 'a t -> int

val elem: int -> 'a t -> 'a

val push: 'a -> 'a t -> unit

val clone: 'a t -> 'a t
