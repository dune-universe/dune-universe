open Core
module CoreInt32_extended = Coreint32_extended.CoreInt32_extended
module CoreInt64_extended = Coreint64_extended.CoreInt64_extended
module Uint64_extended = Uint64_extended.Uint64_extended
module Uint32_extended = Uint32_extended.Uint32_extended
module Uint16_extended = Uint16_extended.Uint16_extended
module Uint8_extended = Uint8_extended.Uint8_extended
(*Types from mysql that are relatively more safely mapped to Ocaml*)
module Types_we_emit = Types_we_emit.Types_we_emit
module Utilities = Utilities.Utilities
module Sql_supported_types = struct
  type t =
      TINYINT
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

  (*--by default just use core int 64 type...*)
  let ml_type_of_supported_sql_type t =
    match t with
    | TINYINT -> Core.Result.Ok Types_we_emit.CoreInt32_extended  (*====TODO===find int8 type or make one *)
    | TINYINT_UNSIGNED -> Core.Result.Ok Types_we_emit.Uint8_extended_t
    | TINYINT_BOOL -> Core.Result.Ok Types_we_emit.Bool
    | SMALLINT_UNSIGNED -> Core.Result.Ok Types_we_emit.Uint16_extended_t
    | INTEGER -> Core.Result.Ok Types_we_emit.CoreInt32_extended
    | INTEGER_UNSIGNED -> Core.Result.Ok Types_we_emit.Uint64_extended_t
    | BIGINT -> Core.Result.Ok Types_we_emit.CoreInt64_extended
    | BIGINT_UNSIGNED -> Core.Result.Ok Types_we_emit.Uint64_extended_t
    | DECIMAL -> Core.Result.Ok Types_we_emit.Bignum
    | FLOAT 
    | DOUBLE -> Core.Result.Ok Types_we_emit.Float
    | DATE -> Core.Result.Ok Types_we_emit.Date
    | DATETIME 
    | TIMESTAMP -> Core.Result.Ok Types_we_emit.Time
    | BINARY
    | BLOB
      (*without checking length of strings we are open to runtime errors or truncation of stored values==TODO==offer a length checked String type*)
    | TINYTEXT
    | TEXT
    | MEDIUMTEXT
    | VARBINARY
    | VARCHAR -> Core.Result.Ok Types_we_emit.String
    (*| ENUM*)
    | UNSUPPORTED -> Core.Result.Error "to_ml_type::UNSUPPORTED_TYPE" 
	
  (*Given the data_type and column_type fields from info schema, determine if the mysql 
    type is supported or not, and if so which type; the data_type field is very easy to 
    match on but lacks the unsigned flag. The unsigned flag is disallowed if not in 
    combination with a numeric type in mysql, so we'll never see it here except with a 
    numeric type. *)
  let of_col_type_and_flags ~data_type ~col_type ~col_name =
    let open Core in 
    let is_unsigned = String.is_substring col_type ~substring:"unsigned" in
    let the_col_type ~is_unsigned ~data_type =
      match is_unsigned, data_type with
      | _, "tinyint" -> if String.is_substring col_name ~substring:"is_" then TINYINT_BOOL else TINYINT_UNSIGNED
      | true, "smallint" -> SMALLINT_UNSIGNED
      | true, "int" 
      | true, "integer" -> INTEGER_UNSIGNED
      | true, "bigint" -> BIGINT_UNSIGNED
      | false, "int" 
      | false, "integer" -> INTEGER
      | false, "bigint" -> BIGINT
      (*Decimals might be signed in mysql, but the bignum type is not; can handle
        signed and unsigned equally well without making distinction between the 
        two*)
      | _, "decimal" -> DECIMAL
      | _, "float"
      | _, "double" -> FLOAT
      | false, "date" -> DATE 
      | false, "datetime" -> DATETIME
      | false, "timestamp" -> TIMESTAMP
      | false, "blob"
      | false, "binary"
      | false, "varbinary"
      | false, "tinytext"
      | false, "text"
      | false, "mediumtext" 
      | false, "varchar" -> VARCHAR
      | _, _ -> let () = Utilities.print_n_flush (String.concat [col_name;" with type ";col_type;" is not supported."])
		in UNSUPPORTED in
    the_col_type ~is_unsigned ~data_type;;
    
  let one_step ~data_type ~col_type ~col_name =
    let supported_t = of_col_type_and_flags ~data_type ~col_type ~col_name in
    let name_result = ml_type_of_supported_sql_type supported_t in
    if is_ok name_result then
      (fun x -> match x with
	       | Core.Result.Ok name -> name
	       | Core.Result.Error _s -> raise (Failure "sql_supported_types::one_step() Unsupported type")) name_result
    else 
      raise (Failure "Unsupported sql type.")
end 
(*  let of_string s =
    match s with
      "tinyint_bool" -> TINYINT_BOOL
    | "tinyint_unsigned" -> TINYINT_UNSIGNED
    | "smallint_unsigned" -> SMALLINT_UNSIGNED
    | "integer" 
    | "int" -> INTEGER
    | "integer_unsigned"
    | "int_unsigned" -> INTEGER_UNSIGNED
    | "bigint" -> BIGINT
    | "bigint_unsigned" -> BIGINT_UNSIGNED 
    | "date" -> DATE
    | "time" 
    | "datetime" -> DATETIME
    | "timestamp" -> TIMESTAMP
    | "blob" -> BLOB
    | "varchar" -> VARCHAR
    | _ -> UNSUPPORTED
end
 *)
