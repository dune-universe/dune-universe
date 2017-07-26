open! Core
open! Async

(** elements with smaller priority number will be poped first *)
type ('elt,'priority) t

val create : ('b -> 'b -> int) -> ('a,'b) t
val length : (_,_) t -> int
val enqueue : ('a,'b) t -> priority:'b -> 'a -> unit
val dequeue : ('a,'b) t -> 'a Deferred.t
val iter_and_clear : ('a,'b) t -> f:('a -> unit) -> unit
val clear : ('a,'b) t -> unit
