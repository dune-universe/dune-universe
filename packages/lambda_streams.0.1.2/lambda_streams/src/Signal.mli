(** Represents a signal that either has data or is terminated. Used for {!Finite} streams. *)

type 'a t =
  | Data of 'a
  | EndOfSignal

val pure : 'a -> 'a t

val empty : unit -> 'a t

(** Get the signal data or the provided default value *)
val default : 'a -> 'a t -> 'a

(** Check whether the signal satisfies the predicate. Returns [false] if it's {!EndOfSignal} *)
val satisfies : ('a -> bool) -> 'a t -> bool

val map : ('a -> 'b) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val from_option : 'a option -> 'a t

val to_option : 'a t -> 'a option
