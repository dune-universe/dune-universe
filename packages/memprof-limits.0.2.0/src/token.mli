(** A flag that can be set atomically, and never unset.

    All the functions below are thread-safe. *)

type t

val create : unit -> t
(** Create a token, initially not set. *)

val set : t -> unit
(** Set a token. *)

val is_set : t -> bool
(** Returns whether {!set} has been called at least once on a given
    token. *)
