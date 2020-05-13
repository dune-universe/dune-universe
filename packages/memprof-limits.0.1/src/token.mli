(** Flag which is settable one-way only *)
type t
val make : unit -> t
val set : t -> unit
val is_set : t -> bool
val release : t -> unit
val armed : t -> bool
