(*Unsigned types do not exist in postgresql, for the most part, unless you create them yourself, and we're not supporting that at present.
See https://stackoverflow.com/questions/20810134/why-unsigned-integer-is-not-available-in-postgresql 
  And serial types are impossible to perfectly represent at present, so we settle for standard 32 and 64 bit types until we can do better.
*)
module Types_we_emit = Types_we_emit.Types_we_emit
module Sql_supported_types : sig 
  type t =
    | BIGINT    (*signed 64 bit integer*)
    | BIGSERIAL (*signed 64 bit integer*)
    | BOOLEAN
    | BYTEA
    | CHAR
    | DATE
    | DECIMAL
    | DOUBLEPRECISION
    | INTEGER   (*signed 32 bit integer*)
    | MONEY
    | NUMERIC
  (*| REAL   unsupported, use double precision floats instead*)
    | SERIAL    (*signed 32 bit integer*)
  (*| SMALLINT   this is a signed 16-bit number, range is -32768 through 32767 *)
  (*| SMALLSERIAL unsupported - range is almost that of unsigned 15-bit, read that again, 15! bit, integer, 1 through 32767. Why! *)
    | TEXT
    | TIME
    | TIMESTAMP
    | VARCHAR
    | UNSUPPORTED

  (*Return a string representation of the ml type we will use to write the module.
    Filter on field name for boolean fields based on prefix? Such as is_xxxx, 
    else is an error unless unsigned tiny int.*)
  val ml_type_of_supported_sql_type : t -> (Types_we_emit.t, string) Core.Result.t
  val of_col_type_and_flags : data_type:string -> col_name:string -> t
  val one_step : data_type:string -> col_name:string -> Types_we_emit.t
end
