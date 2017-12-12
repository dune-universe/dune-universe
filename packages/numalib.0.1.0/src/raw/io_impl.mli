type 'a t = 'a
val return : 'a -> 'a
val bind : 'a -> f:('a -> 'b) -> 'b
val map : 'a -> f:('a -> 'b) -> 'b
val ( >>= ) : 'a -> ('a -> 'b) -> 'b
val ( >>| ) : 'a -> ('a -> 'b) -> 'b
val wrap : 'a -> 'a
