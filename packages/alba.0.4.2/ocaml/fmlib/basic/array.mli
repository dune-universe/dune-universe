
val length: 'a array -> int
val is_empty: 'a array -> bool
val make: int -> 'a -> 'a array
val init: int -> (int -> 'a) -> 'a array
val get: 'a array -> int -> 'a
val set: 'a array -> int -> 'a -> unit
val map: ('a -> 'b) -> 'a array -> 'b array
val mapi: (int -> 'a -> 'b) -> 'a array -> 'b array
val to_list: 'a array -> 'a list
val of_list: 'a list -> 'a array
val blit: 'a array -> int -> 'a array -> int -> int -> unit
val iter: ('a -> unit) -> 'a array -> unit
val copy: 'a array -> 'a array
val sub: 'a array -> int -> int -> 'a array
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a


val first: 'a array -> 'a
val last:  'a array -> 'a
val find: ('a -> bool) -> 'a array -> int
val put: int -> 'a -> 'a array -> 'a array
val take: int -> 'a array -> 'a array
val remove_last: int -> 'a array -> 'a array
val push: 'a -> 'a array -> 'a array
val fill: int -> 'a -> 'a array -> 'a array
