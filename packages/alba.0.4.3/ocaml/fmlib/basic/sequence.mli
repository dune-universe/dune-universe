type 'a t


val map: ('a -> 'b) -> 'a t -> 'b t
val empty: 'a t
val singleton: 'a -> 'a t
val length: 'a t -> int
val is_empty: 'a t -> bool
val elem:   int -> 'a t -> 'a
val put:    int -> 'a -> 'a t -> 'a t
val push:   'a -> 'a t -> 'a t
val push_list: 'a list -> 'a t -> 'a t
val of_list:   'a list -> 'a t
val push_array: 'a array -> 'a t -> 'a t
val of_array:   'a array -> 'a t
val take:   int -> 'a t -> 'a t
val remove_last: int -> 'a t -> 'a t

val to_array: 'a t -> 'a array
val to_string: char t -> string
