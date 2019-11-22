
type 'a t

val create: unit -> 'a t

val insert: (int * 'a) -> 'a t -> unit

val pop: 'a t -> 'a option

val pop_all: 'a t -> 'a list
