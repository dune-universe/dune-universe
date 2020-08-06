
type 'a t

val create: unit -> 'a t

val insert: 'a t -> (int * 'a) -> unit

val pop: 'a t -> 'a option

val pop_all: 'a t -> 'a list
