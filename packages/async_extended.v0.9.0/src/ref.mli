open! Async

type 'a t

val create : 'a -> 'a t

val set : 'a t -> 'a -> unit

val get : 'a t -> 'a

val listen_to_sets : 'a t -> 'a Stream.t

val register :
  'a t
  -> ?init:('a -> unit)
  -> f:('a -> [ `Continue | `Leave ])
  -> unit
