(** {1 Features} *)

(** These functions come from OGR C API's [OGR_F_*] namespace. *)

type t

val t : t Ctypes.typ
val t_opt : t option Ctypes.typ
(** Values for proper Ctypes integration/extension *)

val get_as_integer : t -> int -> int
val get_as_double : t -> int -> float
val get_as_string : t -> int -> string
(** [get_as_* t i] returns field [i] from feature [t]. *)

val get_geometry_ref : t -> Geometry.t
(** [get_geometry_ref t] returns the geometry associated with [t].
    You must keep [t] around for the life of the returned {!Geometry.t}.  Use
    {!get_geometry_copy} to avoid this requirement. *)

val get_geometry_copy : t -> Geometry.t
(** [get_geometry_copy t] returns a copy of the geometry associated with [t].
    It is equivalent to calling [get_geometry_ref t |> Geometry.clone]. *)

val destroy : t -> unit
(** [destroy t] frees the feature [t]. *)

module Defn : sig
  (** {1 Feature Definitions} *)

  (** These functions come from the OGR C API's [OGR_FD_*] namespace. *)

  type t

  val t : t Ctypes.typ
  (** Values for proper Ctypes integration/extension *)

  val get_field_count : t -> int
  (** [get_field_count t] returns the number of fields associated with the
      feature definition [t]. *)

  val get_field_defn : t -> int -> Field.Defn.t
  (** [get_field_defn t i] returns the [i]th {!Field.Defn.t} from [t]. *)
end
