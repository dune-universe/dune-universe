module Bignum_extended = Bignum_extended.Bignum_extended
module Date_extended = Date_extended.Date_extended
module Date_time_extended = Date_time_extended.Date_time_extended
module Postgresql = Postgresql
module Utilities : sig
  val print_n_flush : string -> unit
  val print_n_flush_alist : sep:string -> string list -> unit
  val getcon : ?host:string -> dbname:string -> password:string -> user:string -> Postgresql.connection
  val getcon_defaults : unit -> Postgresql.connection
  val closecon : Postgresql.connection ->  unit
  val serialize_optional_field : field:string option -> conn:Postgresql.connection -> string
  val serialize_optional_field_with_default :
    field:string option -> conn:Postgresql.connection -> default:string -> string
  val serialize_boolean_field : field:bool -> string
  val serialize_optional_bool_field : field:bool option -> string
  val serialize_optional_float_field_as_int : field:float option -> string
  val serialize_float_field_as_int : field:float -> string
  val serialize_optional_date_field : field:Date_extended.t option -> string
  val serialize_optional_date_time_field : field:Date_time_extended.t option -> string
  val parse_boolean_field_exn : field:string -> bool
  (*  val parse_optional_boolean_field_exn : field:string option -> bool option*)

  (*
  val parse_int64_field_exn : string -> Core.Int64.t  
  val parse_int_field_exn : string option -> int
  val parse_int_field_option : string option -> int option
  val parse_string_field : string -> string
  val parse_boolean_field : string -> bool
  val parse_int_field : string -> int*)


  val extract_field_as_string_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> string
  val extract_optional_field :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> string option
  val parse_int64_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Int64.t
  val parse_optional_int64_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Int64.t option
  val parse_int32_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Int32.t
  val parse_optional_int32_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Int32.t option
  val parse_bignum_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Bignum_extended.t
  val parse_optional_bignum_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Bignum_extended.t option
  (*---bool---*)					   
  val parse_bool_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> bool
  val parse_optional_bool_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> bool option
  (*---float/double---*)
  val parse_float_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Float.t
  val parse_optional_float_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Float.t option
  (*---date/time---*)
  val parse_date_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Date.t
  val parse_optional_date_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Date.t option
  val parse_datetime_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Time.t
  val parse_optional_datetime_field_exn :
    fieldname:string -> qresult:Postgresql.result -> tuple:int -> Core.Time.t option
end 
