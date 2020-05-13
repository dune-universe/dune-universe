type 'a t

val make : unit -> 'a t
(** Create an empty map *)

val with_value : 'a t -> value:'a -> ('b -> 'c) -> 'b -> 'c
(** Associate [~value] to the current thread for the duration of a scope.
    It can be nested: the previous association is restored on exit. *)

val get : 'a t -> 'a option
(** Get the value currently associated with the current thread. *)

val reset : 'a t -> unit
(** Reset a map to be empty. *)
