open! Core
open! Async

type 'a t

(** create a min-heap on type 'a *)
val create         : cmp:('a -> 'a -> int) -> 'a t
val push           : 'a t -> 'a -> unit
val pop            : 'a t -> 'a Deferred.t
val length         :  _ t -> int
val iter_and_clear : 'a t -> f:('a -> unit) -> unit
val clear          : 'a t -> unit
