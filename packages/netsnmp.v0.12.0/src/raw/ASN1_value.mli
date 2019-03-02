module Error : sig
  type t =
     No_such_object
   | No_such_instance
   | End_of_mib_view
   | Unknown_error of int

  (** [to_string] converts the error return value to a string *)
  val to_string : t -> string
end

type t =
 | ASN_Null
 | ASN_Error of Error.t
 | ASN_Integer of int
 | ASN_Gauge of int
 | ASN_Counter of int
 | ASN_Timeticks of int
 | ASN_Uinteger of int
 | ASN_String of string
 | ASN_Opaque of string
 | ASN_Ipaddress of string
 | ASN_Objid of Oid.t
 | ASN_Counter64 of ASN1_Counter64.t
 | ASN_Bitstring of ASN1_Bitstring.t
 | ASN_Opaque_counter64 of ASN1_Counter64.t
 | ASN_Opaque_u64 of ASN1_Counter64.t
 | ASN_Opaque_i64 of int64
 | ASN_Opaque_float of float
 | ASN_Opaque_double of float

(** [type_to_string] converts an oid value's type to a string *)
val type_to_string : t -> string

(** [to_string] converts an oid value to a string *)
val to_string : t -> string
