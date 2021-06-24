val nil : 'a list
val cons : 'a -> 'a list -> 'a list
val length : 'a list -> int
val fold : 'r -> ('a -> 'r -> 'r) -> 'a list -> 'r
val fold_rev : 'r -> ('a -> 'r -> 'r) -> 'a list -> 'r
val iter : ('a -> unit) -> 'a list -> unit
val init : int -> 'a -> 'a list
val map : ('a -> 'b) -> 'a list -> 'b list
val conc : 'a list -> 'a list -> 'a list
val flatten : 'a list list -> 'a list
val zip : 'a list -> 'b list -> (unit -> 'r) -> (('a * 'b) list -> 'r) -> 'r
val select : ('a -> 'a -> Order.t) -> int -> 'a list -> 'a
val sort : ('a -> 'a -> Order.t) -> 'a list -> 'a list
val sort_unique : ('a -> 'a -> Order.t) -> 'a list -> 'a list
