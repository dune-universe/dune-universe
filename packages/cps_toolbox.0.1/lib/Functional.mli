val identity : 'a -> 'a
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val swap : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val (<==) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val (==>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
