module Types_we_emit = Types_we_emit.Types_we_emit
module Sql_supported_types : sig 
  type t =
    | TINYINT
    | TINYINT_UNSIGNED
    | TINYINT_BOOL
    | SMALLINT_UNSIGNED
    | INTEGER
    | INTEGER_UNSIGNED
    | BIGINT
    | BIGINT_UNSIGNED
    | DECIMAL
    | FLOAT
    | DOUBLE
    | DATE
    | DATETIME
    | TIMESTAMP
    | BINARY
    | VARBINARY
    | TINYTEXT
    | TEXT
    | MEDIUMTEXT
    | VARCHAR
    | BLOB
  (*| ENUM*)
    | UNSUPPORTED

  (*Return a string representation of the ml type we will use to write the module, 
    such as Uint8.t from Sql_supported_types.TINYINT_UNSIGNED. Filter on field 
    name for boolean fields based on prefix? Such as is_xxxx, else is an error 
    unless unsigned tiny int.*)
  val ml_type_of_supported_sql_type : t -> (Types_we_emit.t, string) Core.Result.t
  val of_col_type_and_flags : data_type:string -> col_type:string -> col_name:string -> t
  val one_step : data_type:string -> col_type:string -> col_name:string -> Types_we_emit.t
end
