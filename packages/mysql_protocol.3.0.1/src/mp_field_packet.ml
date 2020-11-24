
type field_packet_field_type = 
    Field_type_decimal
  | Field_type_tiny
  | Field_type_short
  | Field_type_long
  | Field_type_float
  | Field_type_double
  | Field_type_null
  | Field_type_timestamp
  | Field_type_longlong
  | Field_type_int24
  | Field_type_date
  | Field_type_time
  | Field_type_datetime
  | Field_type_year
  | Field_type_newdate
  | Field_type_varchar
  | Field_type_bit
  | Field_type_newdecimal
  | Field_type_enum
  | Field_type_set
  | Field_type_tiny_blob
  | Field_type_medium_blob
  | Field_type_long_blob
  | Field_type_blob
  | Field_type_var_string
  | Field_type_string
  | Field_type_geometry

let field_packet_field_type_to_string p = 
  let s = 
    match p with
      Field_type_decimal -> "DECIMAL"
    | Field_type_tiny -> "TINY"
    | Field_type_short -> "SHORT"
    | Field_type_long -> "LONG"
    | Field_type_float -> "FLOAT"
    | Field_type_double -> "DOUBLE"
    | Field_type_null -> "NULL"
    | Field_type_timestamp -> "TIMESTAMP"
    | Field_type_longlong ->"LONGLONG"
    | Field_type_int24 -> "INT24"
    | Field_type_date -> "DATE"
    | Field_type_time -> "TIME"
    | Field_type_datetime -> "DATETIME"
    | Field_type_year -> "YEAR"
    | Field_type_newdate -> "NEWDATE"
    | Field_type_varchar -> "VARCHAR"
    | Field_type_bit -> "BIT"
    | Field_type_newdecimal -> "NEWDECIMAL"
    | Field_type_enum -> "ENUM"
    | Field_type_set -> "SET"
    | Field_type_tiny_blob -> "TINYBLOB"
    | Field_type_medium_blob -> "MEDIUMBLOB"
    | Field_type_long_blob -> "LONGBLOB"
    | Field_type_blob -> "BLOB"
    | Field_type_var_string -> "VARSTRING"
    | Field_type_string -> "STRING"
    | Field_type_geometry -> "GEOMETRY"
  in
  s

type field_packet_field_flag = 
    Field_flag_not_null
  | Field_flag_pri_key
  | Field_flag_unique_key
  | Field_flag_multiple_key
  | Field_flag_blob
  | Field_flag_unsigned
  | Field_flag_zerofill
  | Field_flag_binary
  | Field_flag_enum
  | Field_flag_auto_increment
  | Field_flag_timestamp
  | Field_flag_set

let field_packet_field_flag_to_string p = 
  let s = 
    match p with 
      Field_flag_not_null -> "NOT NULL"
    | Field_flag_pri_key -> "PRIMARY KEY"
    | Field_flag_unique_key -> "UNIQUE KEY"
    | Field_flag_multiple_key -> "MULTIPLE KEY"
    | Field_flag_blob -> "BLOB"
    | Field_flag_unsigned -> "UNSIGNED"
    | Field_flag_zerofill -> "ZEROFILL"
    | Field_flag_binary -> "BINARY"
    | Field_flag_enum -> "ENUM"
    | Field_flag_auto_increment -> "AUTO_INCREMENT"
    | Field_flag_timestamp -> "TIMESTAMP"
    | Field_flag_set -> "SET"
  in
  s

type field_packet = {
  field_catalog : string;
  field_db : string;
  field_table : string;
  field_org_table : string;
  field_name : string;
  field_org_name : string;
  field_charset_number : int;
  field_length : Int64.t;
  field_type : field_packet_field_type;
  field_flags : field_packet_field_flag list;
  field_decimals : int;
  field_default : Int64.t;
  version : Mp_protocol.protocol_version
}

let field_packet_to_string p =
  let version = Mp_protocol.protocol_version_to_string p.version in
  let field_type = field_packet_field_type_to_string p.field_type in
  let field_flags = 
    let f acc e = 
      let s = field_packet_field_flag_to_string e in
      acc ^ s ^ " "
    in
    List.fold_left f "" p.field_flags
  in
  let fmt = format_of_string "field_catalog : %s\n"
    ^^ format_of_string "field_db : %s\n"
    ^^ format_of_string "field_table : %s\n"
    ^^ format_of_string "field_org_table : %s\n"
    ^^ format_of_string "field_name : %s\n"
    ^^ format_of_string "field_org_name : %s\n"
    ^^ format_of_string "field_charset_number : %u\n"
    ^^ format_of_string "field_length : %Lu\n"
    ^^ format_of_string "field_type : %s\n"
    ^^ format_of_string "field_flags : %s\n"
    ^^ format_of_string "field_decimals : %u\n"
    ^^ format_of_string "field_default : %Lu\n"
    ^^ format_of_string "version : %s\n"
  in
  Printf.sprintf fmt p.field_catalog
    p.field_db
    p.field_table
    p.field_org_table
    p.field_name
    p.field_org_name
    p.field_charset_number
    p.field_length
    field_type
    field_flags
    p.field_decimals
    p.field_default
    version

let decode_field_packet_field_type field_type = 
  match field_type with
    0x00 -> Field_type_decimal
  | 0x01 -> Field_type_tiny
  | 0x02 -> Field_type_short
  | 0x03 -> Field_type_long
  | 0x04 -> Field_type_float
  | 0x05 -> Field_type_double
  | 0x06 -> Field_type_null
  | 0x07 -> Field_type_timestamp
  | 0x08 -> Field_type_longlong
  | 0x09 -> Field_type_int24
  | 0x0a -> Field_type_date
  | 0x0b -> Field_type_time
  | 0x0c -> Field_type_datetime
  | 0x0d -> Field_type_year
  | 0x0e -> Field_type_newdate
  | 0x0f -> Field_type_varchar
  | 0x10 -> Field_type_bit
  | 0xf6 -> Field_type_newdecimal
  | 0xf7 -> Field_type_enum
  | 0xf8 -> Field_type_set
  | 0xf9 -> Field_type_tiny_blob
  | 0xfa -> Field_type_medium_blob
  | 0xfb -> Field_type_long_blob
  | 0xfc -> Field_type_blob
  | 0xfd -> Field_type_var_string
  | 0xfe -> Field_type_string
  | 0xff -> Field_type_geometry
  | _ -> failwith (Printf.sprintf "Unknown field type = %u in field packet" field_type)

let decode_field_packet_field_flag bits = 
  (* /!\ : bits is 1 or 2 bytes *)
  let length = Bitstring.bitstring_length bits in
  let (binary, zerofill, unsigned, blob, multiple_key, unique_key, pri_key, not_null,
       set, timestamp, auto_increment, enum) = 
    if length = 2*8 then
      match%bitstring bits with
      | {| binary : 1;
          zerofill : 1;
          unsigned : 1;
          blob : 1;
          multiple_key : 1;
          unique_key : 1;
          pri_key : 1;
          not_null : 1;
          _ : 1;
          _ : 1;
          _ : 1;
          _ : 1;
          set : 1;
          timestamp : 1;
          auto_increment : 1;
          enum : 1
        |} ->
          (binary, zerofill, unsigned, blob, multiple_key, unique_key, pri_key, not_null,
            set, timestamp, auto_increment, enum)
    else if length = 1*8 then
      match%bitstring bits with
      | {| binary : 1;
          zerofill : 1;
          unsigned : 1;
          blob : 1;
          multiple_key : 1;
          unique_key : 1;
          pri_key : 1;
          not_null : 1
        |} ->
          (binary, zerofill, unsigned, blob, multiple_key, unique_key, pri_key, not_null,
            false, false, false, false)
    else 
      failwith (Printf.sprintf "Bad length = %u for field flags in field packet" length)
  in
  let l = [] in
  let l = if binary then Field_flag_binary::l else l in
  let l = if zerofill then Field_flag_zerofill::l else l in
  let l = if unsigned then Field_flag_unsigned::l else l in
  let l = if blob then Field_flag_blob::l else l in
  let l = if multiple_key then Field_flag_multiple_key::l else l in
  let l = if unique_key then Field_flag_unique_key::l else l in
  let l = if pri_key then Field_flag_pri_key::l else l in
  let l = if not_null then Field_flag_not_null::l else l in
  let l = if set then Field_flag_set::l else l in
  let l = if timestamp then Field_flag_timestamp::l else l in
  let l = if auto_increment then Field_flag_auto_increment::l else l in
  let l = if enum then Field_flag_enum::l else l in
  l

let field_packet acc ic oc =
  let (_, _, bits) = Mp_packet.extract_packet ic oc in
  let (catalog, rest) = Mp_string.length_coded_string bits in
  let field = 
    if catalog = "def" then
      let (db, rest) = Mp_string.length_coded_string rest in
      let (table, rest) = Mp_string.length_coded_string rest in
      let (org_table, rest) = Mp_string.length_coded_string rest in
      let (name, rest) = Mp_string.length_coded_string rest in
      let (org_name, rest) = Mp_string.length_coded_string rest in
      let length_rest = (Bitstring.bitstring_length rest) - ((9*8) + Mp_bitstring.compute32) in
      match%bitstring rest with 
      | {| _ : 1*8 : int, unsigned, bigendian; (* filler *)
          charset_number : 2*8 : int, unsigned, littleendian;
          length : Mp_bitstring.compute32 : int, unsigned, littleendian;
          field_type : 1*8 : int, unsigned, bigendian;
          flags : 2*8 : bitstring;
          decimals : 1*8 : int, unsigned, bigendian;
          0x00 : 1*8 : int, unsigned, bigendian;
          0x00 : 1*8 : int, unsigned, bigendian;
          rest : length_rest : bitstring |} -> (
            let (default, _) = 
              if (Bitstring.bitstring_length rest > 0) then 
                Mp_binary.length_coded_binary rest
              else
                (Int64.zero, rest)
            in
            {
              field_catalog = catalog;
              field_db = db;
              field_table = table;
              field_org_table = org_table;
              field_name = name;
              field_org_name = org_name;
              field_charset_number = charset_number;
              field_length = length;
              field_type = decode_field_packet_field_type field_type;
              field_flags = decode_field_packet_field_flag flags;
              field_decimals = decimals;
              field_default = default;
              version = Mp_protocol.Protocol_version_41
            }
          )
    else
      (* /!\ : Wiki description IS NOT right
	      see protocol.cc files in MySQL source code *)
      let (field_table, rest) = Mp_string.length_coded_string bits in
      let (field_name, rest) = Mp_string.length_coded_string rest in
      let length_rest = (Bitstring.bitstring_length rest) - (7*8) in
      match%bitstring rest with
      | {| 3 : 1*8 : int, unsigned, bigendian;
          length : 3*8 : int, unsigned, littleendian;
          1: 1*8 : int, unsigned, bigendian;
          field_type : 1*8 : int, unsigned, bigendian;
          head_flags : 1*8 : int, unsigned, bigendian;
          rest : length_rest : bitstring |} ->
            let (flags, rest) = 
              if (head_flags = 2) then
                let length_rest = (Bitstring.bitstring_length rest) - 8 in
                match%bitstring rest with
                | {| flags : 1*8 : bitstring;
                    rest : length_rest : bitstring |} -> (flags, rest)
              else if (head_flags = 3) then
                let length_rest = (Bitstring.bitstring_length rest) - (2*8) in
                match%bitstring rest with 
                | {| flags : 2*8 : bitstring;
                    rest : length_rest : bitstring |} -> (flags, rest)
              else
                failwith (Printf.sprintf "Bad head_flags = %u in field packet" head_flags)
            in
            let length_rest = (Bitstring.bitstring_length rest) - 8 in
            match%bitstring rest with 
            | {| decimals : 1*8 : int, unsigned, bigendian;
                rest : length_rest : bitstring |} -> (
                  let (default, _) = 
                    if Bitstring.bitstring_length rest > 0 then 
                      Mp_binary.length_coded_binary rest 
                    else
                      (Int64.zero, Bitstring.empty_bitstring)
                  in
                  { field_catalog = "";
                    field_db = "";
                    field_table = field_table;
                    field_org_table = "";
                    field_name = field_name;
                    field_org_name = "";
                    field_charset_number = 0;
                    field_length = (Int64.of_int length);
                    field_type = decode_field_packet_field_type field_type;
                    field_flags = decode_field_packet_field_flag flags;
                    field_decimals = decimals;
                    field_default = default;
                    version = Mp_protocol.Protocol_version_40 }
                )
  in
  acc := field :: !acc
