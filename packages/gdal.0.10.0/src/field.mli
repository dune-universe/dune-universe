(** {1 OGR Fields} *)

module Defn : sig
  (** {1 OGR Field Definitions} *)

  (** These are bindings and wrappers around the [OGR_Fld_*] API. *)

  type t
  (** Field definition *)

  val t : t Ctypes.typ
  (** Values for proper Ctypes integration/extension *)

  type field_type_t =
    | Integer
    | IntegerList
    | Real
    | RealList
    | String
    | StringList
    | WideString
    | WideStringList
    | Binary
    | Date
    | Time
    | DateTime
    (** Possible field types *)

  val get_type : t -> field_type_t
  (** [get_type t] returns the data type associated with the field
      definition [t]. *)

  val get_name : t -> string
  (** [get_name t] returns the name associated with the field
      definition [t]. *)
end
