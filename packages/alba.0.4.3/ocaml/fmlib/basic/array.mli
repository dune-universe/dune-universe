open Module_types


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
val fold_right: ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b

val foldi_left: ('a -> int -> 'b -> 'a) -> 'a -> 'b array -> 'a
val foldi_right: (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b


val first: 'a array -> 'a
val last:  'a array -> 'a
val find: ('a -> bool) -> 'a array -> int
val put: int -> 'a -> 'a array -> 'a array
val take: int -> 'a array -> 'a array
val remove_last: int -> 'a array -> 'a array
val push: 'a -> 'a array -> 'a array
val fill: int -> 'a -> 'a array -> 'a array



module Monadic (M: MONAD):
sig
    val mapi: (int -> 'a -> 'b M.t) -> 'a array -> 'b array M.t

    val map: ('a -> 'b M.t) -> 'a array -> 'b array M.t

    val fold_left: ('a -> 'b -> 'b M.t) -> 'a array -> 'b -> 'b M.t

    val foldi_left: (int -> 'a -> 'b -> 'b M.t) -> 'a array -> 'b -> 'b M.t
end
