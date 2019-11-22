module Error = struct
  type t =
     No_such_object
   | No_such_instance
   | End_of_mib_view
   | Unknown_error of int

  let to_string = function
   | No_such_object    -> "No such object"
   | No_such_instance  -> "No such instance"
   | End_of_mib_view   -> "End of MIB view"
   | Unknown_error e   -> "Unknown error (" ^ (string_of_int e) ^ ")"

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

let type_to_string v =
  match v with
  | ASN_Null                 -> "Null"
  | ASN_Integer _            -> "Integer"
  | ASN_Gauge _              -> "Guage"
  | ASN_Counter _            -> "Counter"
  | ASN_Timeticks _          -> "Timeticks"
  | ASN_Uinteger _           -> "Uinteger"
  | ASN_String _             -> "String"
  | ASN_Opaque _             -> "Opaque"
  | ASN_Ipaddress _          -> "Ipaddress"
  | ASN_Objid _              -> "Objid"
  | ASN_Counter64 _          -> "Counter64"
  | ASN_Bitstring _          -> "Bitstring"
  | ASN_Opaque_counter64 _   -> "Opaque_counter64"
  | ASN_Opaque_u64 _         -> "Opaque_u64"
  | ASN_Opaque_i64 _         -> "Opaque_i64"
  | ASN_Opaque_float _       -> "Opaque_float"
  | ASN_Opaque_double _      -> "Opaque_double"
  | ASN_Error _oid_value_error -> "ERROR"

let to_string v =
  match v with
  | ASN_Null                 -> "Null"
  | ASN_Integer v            -> string_of_int v
  | ASN_Gauge v              -> string_of_int v
  | ASN_Counter v            -> string_of_int v
  | ASN_Timeticks v          -> string_of_int v
  | ASN_Uinteger v           -> string_of_int v
  | ASN_String s             -> s
  | ASN_Opaque s             -> s
  | ASN_Ipaddress s          -> s
  | ASN_Objid o              -> Mib.snprint_objid o
  | ASN_Counter64 v          -> ASN1_Counter64.to_string v
  | ASN_Bitstring s          -> ASN1_Bitstring.to_string s
  | ASN_Opaque_counter64 v   -> ASN1_Counter64.to_string v
  | ASN_Opaque_u64 v         -> ASN1_Counter64.to_string v
  | ASN_Opaque_i64 v         -> Int64.to_string v
  | ASN_Opaque_float f       -> string_of_float f
  | ASN_Opaque_double f      -> string_of_float f 
  | ASN_Error err            -> Error.to_string err
