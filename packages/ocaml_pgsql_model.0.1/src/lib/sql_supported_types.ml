(*Unsigned types do not exist in postgresql, for the most part, unless you create them yourself, and we're not supporting that at present.
  See https://stackoverflow.com/questions/20810134/why-unsigned-integer-is-not-available-in-postgresql *)
open Core
module Types_we_emit = Types_we_emit.Types_we_emit
module Utilities = Utilities.Utilities
module Sql_supported_types = struct
  type t =
    | BIGINT (*is just a 64-bit signed int*)
    | BIGSERIAL (*zero is not permitted!!!*)
    | BOOLEAN
    | BYTEA
    | CHAR
    | DATE
    | DECIMAL
    | DOUBLEPRECISION
    | INTEGER (*signed 32 bit integer, -2147483648 through +214743647*)
    | MONEY
    | NUMERIC
 (* | REAL unsupported - use DOUBLE precision floats instead*)
    | SERIAL (*this is almost an unsigned 32 bit integer except zero is not a valid value*)
 (* | SMALLINT this is a signed 16-bit number, range is -32768 through 32767 *)
 (* | SMALLSERIAL unsupported - range is almost that of unsigned 15-bit, read that again, 15! bit, integer, 1 through 32767. Why! *)
    | TEXT
    | TIME
    | TIMESTAMP
    | VARCHAR
    | UNSUPPORTED

  let ml_type_of_supported_sql_type t =
    match t with 
    | BIGINT -> Core.Result.Ok Types_we_emit.CoreInt64
    | BIGSERIAL -> Core.Result.Ok Types_we_emit.CoreInt64
    | BOOLEAN -> Core.Result.Ok Types_we_emit.Bool
    | BYTEA
    | CHAR -> Core.Result.Ok Types_we_emit.String
    | DATE -> Core.Result.Ok Types_we_emit.Date
    | DECIMAL -> Core.Result.Ok Types_we_emit.Bignum
    | DOUBLEPRECISION -> Core.Result.Ok Types_we_emit.Float      
    | INTEGER -> Core.Result.Ok Types_we_emit.CoreInt32
    | MONEY -> Core.Result.Ok Types_we_emit.Bignum
    | NUMERIC -> Core.Result.Ok Types_we_emit.Bignum
  (*| REAL -> unsupported - use DOUBLE precision and floats *)
    | SERIAL -> Core.Result.Ok Types_we_emit.CoreInt32
  (*| SMALLINT ->
    | SMALLSERIAL unsupported *)
    | TEXT -> Core.Result.Ok Types_we_emit.String
    | TIME -> Core.Result.Ok Types_we_emit.Time
    | TIMESTAMP -> Core.Result.Ok Types_we_emit.Time
    | VARCHAR -> Core.Result.Ok Types_we_emit.String
    | UNSUPPORTED -> Core.Result.Error "to_ml_type::UNSUPPORTED_TYPE"

  (*Given the data_type and column_type fields from info schema, determine if the data
    type is supported or not, and if so which type; the data_type field is very easy to 
    match on. NOTE that unsigned data types DO NOT EXIST in postgresql, unlike mysql. *)
  let of_col_type_and_flags ~data_type ~col_name =
    let open Core in 
    let the_col_type ~data_type =
      match data_type with
      | "bigint" -> BIGINT
      | "bigserial" -> BIGINT
      | "boolean" -> BOOLEAN
      | "bytea" -> BYTEA
      | "char" -> CHAR
      | "date" -> DATE
      | "decimal" -> DECIMAL
      | "double precision" -> DOUBLEPRECISION
      | "integer" -> INTEGER
      | "money" -> MONEY
      | "numeric" -> NUMERIC
      | "serial" -> SERIAL
      | "text" -> TEXT
      | "time" -> TIMESTAMP
      | "timestamp" -> TIMESTAMP
      | "character varying" -> VARCHAR
      | _ -> let () = Utilities.print_n_flush (String.concat [col_name;" with type ";data_type;" is not supported."])
	     in UNSUPPORTED in
    the_col_type ~data_type;;

  let one_step ~data_type ~col_name =
    let supported_t = of_col_type_and_flags ~data_type ~col_name in
    let name_result = ml_type_of_supported_sql_type supported_t in
    if is_ok name_result then
      (fun x -> match x with
	       | Core.Result.Ok name -> name
	       | Core.Result.Error _s -> raise (Failure "sql_supported_types::one_step() Unsupported type")) name_result
    else 
      raise (Failure "Unsupported sql type.")
end 
