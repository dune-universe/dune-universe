type 'a t

(** Create an empty mvar. *)
val create_empty : unit -> 'a t

(** Create an mvar containing a value. *)
val create : 'a -> 'a t

(** Wait until mvar holds a value, then return it and leave the mvar empty. *)
val take : 'a t -> 'a

(** Non-blocking take - if the mvar holds no value, return None. *)
val try_take : 'a t -> 'a option

(** Wait until the mvar is empty, then put the supplied value in the mvar. *)
val put : 'a t -> 'a -> unit

(** Non-blocking put - if the mvar is empty the put the supplied value in the
    mvar, otherwise return false. *)
val try_put : 'a t -> 'a -> bool

(** Test whether the mvar is empty. *)
val is_empty : 'a t -> bool

(** Wait until the mvar is populated, then put the supplied value in the mvar
    and return the previous value. *)
val swap : 'a t -> 'a -> 'a

(** Wait until the mvar is populated, then use the supplied function
    to modify its value.*)
val modify : 'a t -> ('a -> 'a) -> unit
