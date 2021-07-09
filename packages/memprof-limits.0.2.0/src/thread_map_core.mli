(* a thread-local store *)
type 'a t
val create : unit -> 'a t
val get : 'a t -> 'a option
val set : 'a t -> 'a option -> unit
val clear : 'a t -> unit
