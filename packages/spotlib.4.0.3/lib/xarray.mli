val foldi_left : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a array -> 'acc
val foldi_right : (int -> 'a -> 'acc -> 'acc) -> 'a array -> 'acc -> 'acc

val shuffle : ?random:(int -> int) -> 'a array -> unit
