val none : 'a option
val some : 'a -> 'a option
val map : ('a -> 'b) -> 'a option -> 'b option
val map2 : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
