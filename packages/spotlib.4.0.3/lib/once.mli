(** Reference cell initializable only once *)

type 'a t

exception Already_initialized

val create : unit -> 'a t
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a option
