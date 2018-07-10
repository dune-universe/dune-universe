(** {1 Drivers} *)

exception Invalid_driver

type t
val t : t Ctypes.typ

val get_short_name : t -> string
val get_long_name : t -> string
(** [get_*_name t] returns the name associated with [t]. *)

val get_by_name : string -> t option
(** [get_by_name name] returns the driver associated with [name] or [None]
    if no driver matches [name]. *)

val get_by_name_exn : string -> t
(** Like {!get_by_name} except that it raises {!Invalid_driver} if no matching
    driver is found. *)

val identify : ?options:string list -> string -> t option
(** [identify ?options name] will try to identify a driver appropriate for the
    data source [name]. *)
