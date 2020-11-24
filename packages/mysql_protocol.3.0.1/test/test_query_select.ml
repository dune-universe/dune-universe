open OUnit
open Mysql_protocol

let print_warning=false

let build_mysql_field ~db_name ~table ~f ~cn ~fl ~ft ~flags ~decimals ~def = 
  {
    Mp_field_packet.field_catalog = "def";
    Mp_field_packet.field_db = db_name;
    Mp_field_packet.field_table = table;
    Mp_field_packet.field_org_table = table;
    Mp_field_packet.field_name = f;
    Mp_field_packet.field_org_name = f;
    Mp_field_packet.field_charset_number = cn;
    Mp_field_packet.field_length = fl;
    Mp_field_packet.field_type = ft;
    Mp_field_packet.field_flags = flags;
    Mp_field_packet.field_decimals = decimals;
    Mp_field_packet.field_default = Int64.of_int def;
    Mp_field_packet.version = Mp_protocol.Protocol_version_41;
  }

let mysql_fields vendor db_name charset version = 
  (* the field length depends on the charset *)
  let fl nb8bits = 
    let (encoding, _) = Mp_charset.number_charset charset in
    match encoding with
    | Mp_charset.Utf8 -> Int64.mul nb8bits (Int64.of_int 3)
    | _ -> nb8bits
  in
  let flags_timestamp_null_no_def =
    match vendor with
    | Test_types.MySQL -> (
        if version >= 5611 then
          [Mp_field_packet.Field_flag_timestamp; Mp_field_packet.Field_flag_not_null; Mp_field_packet.Field_flag_binary;] 
        else
          [Mp_field_packet.Field_flag_timestamp; Mp_field_packet.Field_flag_not_null; 
            Mp_field_packet.Field_flag_unsigned; Mp_field_packet.Field_flag_zerofill; Mp_field_packet.Field_flag_binary;]
      )
    | Test_types.MariaDB -> (
        [Mp_field_packet.Field_flag_timestamp; Mp_field_packet.Field_flag_not_null;
          Mp_field_packet.Field_flag_unsigned; Mp_field_packet.Field_flag_binary;] 
    )
  in
  let flags_timestamp_not_null_def20110510 =
    match vendor with
    | Test_types.MySQL -> (
        if version >= 5611 then
          [Mp_field_packet.Field_flag_not_null; Mp_field_packet.Field_flag_binary;] 
        else
          [Mp_field_packet.Field_flag_not_null; Mp_field_packet.Field_flag_unsigned; 
           Mp_field_packet.Field_flag_zerofill; Mp_field_packet.Field_flag_binary;]
      )
    | Test_types.MariaDB -> (
        [Mp_field_packet.Field_flag_not_null;
          Mp_field_packet.Field_flag_unsigned; Mp_field_packet.Field_flag_binary;] 
    )
  in
  let fl_time_null_no_def = if version >= 5611 then 10 else 8 in
  let fl_time_not_null_def214702 = if version >= 5611 then 10 else 8 in
  [
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_autoinc_not_null_no_def" ~cn:63 ~fl:(Int64.of_int 20)
      ~ft:Mp_field_packet.Field_type_longlong 
      (* flags order is important to pass the test *)
      ~flags:[Mp_field_packet.Field_flag_auto_increment; Mp_field_packet.Field_flag_not_null; Mp_field_packet.Field_flag_pri_key] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int_null_no_def" ~cn:63 ~fl:(Int64.of_int 11)
      ~ft:Mp_field_packet.Field_type_long
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_smallint_null_no_def" ~cn:63 ~fl:(Int64.of_int 6)
      ~ft:Mp_field_packet.Field_type_short
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_decimal_12_4_null_no_def" ~cn:63 ~fl:(Int64.of_int 14)
      ~ft:Mp_field_packet.Field_type_newdecimal
      ~flags:[] 
      ~decimals:4 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_datetime_null_no_def" ~cn:63 ~fl:(Int64.of_int 19)
      ~ft:Mp_field_packet.Field_type_datetime
      ~flags:[Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_float_null_no_def" ~cn:63 ~fl:(Int64.of_int 17)
      ~ft:Mp_field_packet.Field_type_float
      ~flags:[] 
      ~decimals:9 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_double_null_no_def" ~cn:63 ~fl:(Int64.of_int 24)
      ~ft:Mp_field_packet.Field_type_double
      ~flags:[] 
      ~decimals:6 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int24_null_no_def" ~cn:63 ~fl:(Int64.of_int 9)
      ~ft:Mp_field_packet.Field_type_int24
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_date_null_no_def" ~cn:63 ~fl:(Int64.of_int 10)
      ~ft:Mp_field_packet.Field_type_date
      ~flags:[Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_time_null_no_def" ~cn:63 ~fl:(Int64.of_int fl_time_null_no_def)
      ~ft:Mp_field_packet.Field_type_time
      ~flags:[Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_year_null_no_def" ~cn:63 ~fl:(Int64.of_int 4)
      ~ft:Mp_field_packet.Field_type_year
      ~flags:[Mp_field_packet.Field_flag_unsigned; Mp_field_packet.Field_flag_zerofill;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_string_null_no_def" ~cn:charset ~fl:(fl (Int64.of_int 150))
      ~ft:Mp_field_packet.Field_type_string
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_varstring_null_no_def" ~cn:charset ~fl:(fl (Int64.of_int 250))
      ~ft:Mp_field_packet.Field_type_var_string
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_blobtext_null_no_def" ~cn:charset ~fl:(fl (Int64.of_int 65535))
      ~ft:Mp_field_packet.Field_type_blob
      ~flags:[Mp_field_packet.Field_flag_blob;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_blobblob_null_no_def" ~cn:63 ~fl:(Int64.of_int 65535)
      ~ft:Mp_field_packet.Field_type_blob
      ~flags:[Mp_field_packet.Field_flag_blob; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_blobtiny_null_no_def" ~cn:63 ~fl:(Int64.of_int 255)
      ~ft:Mp_field_packet.Field_type_blob
      ~flags:[Mp_field_packet.Field_flag_blob; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_blobmedium_null_no_def" ~cn:63 ~fl:(Int64.of_int 16777215)
      ~ft:Mp_field_packet.Field_type_blob
      ~flags:[Mp_field_packet.Field_flag_blob; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_bloblong_null_no_def" ~cn:63 ~fl:(Int64.of_string "4294967295")
      ~ft:Mp_field_packet.Field_type_blob
      ~flags:[Mp_field_packet.Field_flag_blob; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_blobimg_null_no_def" ~cn:63 ~fl:(Int64.of_int 65535)
      ~ft:Mp_field_packet.Field_type_blob
      ~flags:[Mp_field_packet.Field_flag_blob; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_enum_null_no_def" ~cn:charset ~fl:(fl (Int64.of_int 5))
      ~ft:Mp_field_packet.Field_type_string
      ~flags:[Mp_field_packet.Field_flag_enum;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_set_null_no_def" ~cn:charset ~fl:(fl (Int64.of_int 14))
      ~ft:Mp_field_packet.Field_type_string
      ~flags:[Mp_field_packet.Field_flag_set;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int_default_null" ~cn:63 ~fl:(Int64.of_int 11)
      ~ft:Mp_field_packet.Field_type_long
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_timestamp_null_no_def" ~cn:63 ~fl:(Int64.of_int 19)
      ~ft:Mp_field_packet.Field_type_timestamp
      ~flags:flags_timestamp_null_no_def
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_bit_null_no_def" ~cn:63 ~fl:(Int64.of_int 64)
      ~ft:Mp_field_packet.Field_type_bit
      ~flags:[Mp_field_packet.Field_flag_unsigned;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_tinyint_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 4)
      ~ft:Mp_field_packet.Field_type_tiny
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_tinyint_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 3)
      ~ft:Mp_field_packet.Field_type_tiny
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 11)
      ~ft:Mp_field_packet.Field_type_long
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 10)
      ~ft:Mp_field_packet.Field_type_long
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_smallint_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 6)
      ~ft:Mp_field_packet.Field_type_short
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_smallint_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 5)
      ~ft:Mp_field_packet.Field_type_short
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_decimal_65_20_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 67)
      ~ft:Mp_field_packet.Field_type_newdecimal
      ~flags:[] 
      ~decimals:20 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_decimal_65_20_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 66)
      ~ft:Mp_field_packet.Field_type_newdecimal
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:20 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_float_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 12)
      ~ft:Mp_field_packet.Field_type_float
      ~flags:[] 
      ~decimals:31 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_float_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 12)
      ~ft:Mp_field_packet.Field_type_float
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:31 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_double_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 22)
      ~ft:Mp_field_packet.Field_type_double
      ~flags:[] 
      ~decimals:31 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_double_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 22)
      ~ft:Mp_field_packet.Field_type_double
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:31 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int24_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 9)
      ~ft:Mp_field_packet.Field_type_int24
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int24_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 8)
      ~ft:Mp_field_packet.Field_type_int24
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_bigint_null_no_def_signed" ~cn:63 ~fl:(Int64.of_int 20)
      ~ft:Mp_field_packet.Field_type_longlong 
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_bigint_null_no_def_unsigned" ~cn:63 ~fl:(Int64.of_int 20)
      ~ft:Mp_field_packet.Field_type_longlong 
      ~flags:[Mp_field_packet.Field_flag_unsigned] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_string_default_null" ~cn:charset ~fl:(fl (Int64.of_int 10))
      ~ft:Mp_field_packet.Field_type_string
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_date_not_null_def20110101" ~cn:63 ~fl:(Int64.of_int 10)
      ~ft:Mp_field_packet.Field_type_date
      ~flags:[Mp_field_packet.Field_flag_not_null; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_datetime_not_null_def20111011140534" ~cn:63 ~fl:(Int64.of_int 19)
      ~ft:Mp_field_packet.Field_type_datetime
      ~flags:[Mp_field_packet.Field_flag_not_null; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_timestamp_not_null_def20110510" ~cn:63 ~fl:(Int64.of_int 19)
      ~ft:Mp_field_packet.Field_type_timestamp
      ~flags:flags_timestamp_not_null_def20110510
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_time_not_null_def214702" ~cn:63 ~fl:(Int64.of_int fl_time_not_null_def214702)
      ~ft:Mp_field_packet.Field_type_time
      ~flags:[Mp_field_packet.Field_flag_not_null; Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_big_enum_default_null" ~cn:charset ~fl:(fl (Int64.of_int 3))
      ~ft:Mp_field_packet.Field_type_string
      ~flags:[Mp_field_packet.Field_flag_enum;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_binary_default_null" ~cn:63 ~fl:(Int64.of_int 123)
      ~ft:Mp_field_packet.Field_type_string
      ~flags:[Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_varbinary_default_null" ~cn:63 ~fl:(Int64.of_int 381)
      ~ft:Mp_field_packet.Field_type_var_string
      ~flags:[Mp_field_packet.Field_flag_binary;] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_bigint_def_0" ~cn:63 ~fl:(Int64.of_int 20)
      ~ft:Mp_field_packet.Field_type_longlong 
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int_def_0" ~cn:63 ~fl:(Int64.of_int 11)
      ~ft:Mp_field_packet.Field_type_long
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_smallint_def_0" ~cn:63 ~fl:(Int64.of_int 6)
      ~ft:Mp_field_packet.Field_type_short
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_decimal_20_9_def_0" ~cn:63 ~fl:(Int64.of_int 22)
      ~ft:Mp_field_packet.Field_type_newdecimal
      ~flags:[] 
      ~decimals:9 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_float_def_0" ~cn:63 ~fl:(Int64.of_int 12)
      ~ft:Mp_field_packet.Field_type_float
      ~flags:[] 
      ~decimals:31 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_double_def_0" ~cn:63 ~fl:(Int64.of_int 22)
      ~ft:Mp_field_packet.Field_type_double
      ~flags:[] 
      ~decimals:31 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_int24_def_0" ~cn:63 ~fl:(Int64.of_int 9)
      ~ft:Mp_field_packet.Field_type_int24
      ~flags:[] 
      ~decimals:0 ~def:0;
    build_mysql_field ~db_name:db_name ~table:"test_ocmp" ~f:"f_tinyint_def_0" ~cn:63 ~fl:(Int64.of_int 4)
      ~ft:Mp_field_packet.Field_type_tiny
      ~flags:[] 
      ~decimals:0 ~def:0;
  ]

let fields = [
  ("f_autoinc_not_null_no_def", 0);
  ("f_int_null_no_def", 1);
  ("f_smallint_null_no_def", 2);
  ("f_decimal_12_4_null_no_def", 3);
  ("f_datetime_null_no_def", 4);
  ("f_float_null_no_def", 5);
  ("f_double_null_no_def", 6);
  ("f_int24_null_no_def", 7);
  ("f_date_null_no_def", 8);
  ("f_time_null_no_def", 9);
  ("f_year_null_no_def", 10);
  ("f_string_null_no_def", 11);
  ("f_varstring_null_no_def", 12);
  ("f_blobtext_null_no_def", 13);
  ("f_blobblob_null_no_def", 14);
  ("f_blobtiny_null_no_def", 15);
  ("f_blobmedium_null_no_def", 16);
  ("f_bloblong_null_no_def", 17);
  ("f_blobimg_null_no_def", 18);
  ("f_enum_null_no_def", 19);
  ("f_set_null_no_def", 20);
  ("f_int_default_null", 21);
  ("f_timestamp_null_no_def", 22);
  ("f_bit_null_no_def", 23);
  ("f_tinyint_null_no_def_signed", 24);
  ("f_tinyint_null_no_def_unsigned", 25);
  ("f_int_null_no_def_signed", 26);
  ("f_int_null_no_def_unsigned", 27);
  ("f_smallint_null_no_def_signed", 28);
  ("f_smallint_null_no_def_unsigned", 29);
  ("f_decimal_65_20_null_no_def_signed", 30);
  ("f_decimal_65_20_null_no_def_unsigned", 31);
  ("f_float_null_no_def_signed", 32);
  ("f_float_null_no_def_unsigned", 33);
  ("f_double_null_no_def_signed", 34);
  ("f_double_null_no_def_unsigned", 35);
  ("f_int24_null_no_def_signed", 36);
  ("f_int24_null_no_def_unsigned", 37);
  ("f_bigint_null_no_def_signed", 38);
  ("f_bigint_null_no_def_unsigned", 39);
  ("f_string_default_null", 40);
  ("f_date_not_null_def20110101", 41);
  ("f_datetime_not_null_def20111011140534", 42);
  ("f_timestamp_not_null_def20110510", 43);
  ("f_time_not_null_def214702", 44);
  ("f_big_enum_default_null", 45);
  ("f_binary_default_null", 46);
  ("f_varbinary_default_null", 47);
  ("f_bigint_def_0", 48);
  ("f_int_def_0", 49);
  ("f_smallint_def_0", 50);
  ("f_decimal_20_9_def_0", 51);
  ("f_float_def_0", 52);
  ("f_double_def_0", 53);
  ("f_int24_def_0", 54);
  ("f_tinyint_def_0", 55);
]

let fields_equals l1 l2 =
  l1 = l2

let data_to_string d =
  let s = Mp_data.to_string d in
  match s with
  | None -> "NULL"
  | Some s -> s

let data_equals d1 d2 =
  let f : bool -> Mp_data.t -> Mp_data.t -> bool = fun acc e1 e2 ->
    let comparison = Mp_data.eq e1 e2 in
    let comparison = 
      let comparison_float_double () =
        let () = 
          if (print_warning) then (
            prerr_endline (Printf.sprintf "Warning: string comparison for float/double: e1=%s e2=%s" 
                             (data_to_string e1) (data_to_string e2))
          )
        in
        let se1 = data_to_string e1 in
        let se2 = data_to_string e2 in
        let se1 =
          if (se1 = "3.402823466e+38") then 
            let () = 
              if (print_warning) then
                prerr_endline "Warning: force 3.402823466e+38 to 3.40282e+38 to pass the test"
            in
            "3.40282e+38"
          else if (se1 = "-1.175494351e-38") then 
            let () = 
              if (print_warning) then
                prerr_endline "Warning: force -1.175494351e-38 to -1.17549e-38 to pass the test"
            in
            "-1.17549e-38"
          else if (se1 = "3.402823466e+38") then 
            let () = 
              if (print_warning) then
                prerr_endline "Warning: force 3.402823466e+38 to 3.40282346639e+38 to pass the test"
            in
            "3.40282346639e+38"
          else if (se1 = "-75618.93781") then 
            let () = 
              if (print_warning) then
                prerr_endline "Warning: force -75618.93781 to -75618.9 to pass the test"
            in
            "-75618.9"
          else if (se1 = "1.79769313486e+308") then 
            let () = 
              if (print_warning) then
                prerr_endline "Warning: force -75618.93781 to \"inf\" to pass the test"
            in
            "inf"
          else
            se1
        in
        let se1 = 
          if (se1 <> se2) then
            if (se1 = "3.402823466e+38" || se1 = "3.40282e+38") then 
              let () = 
                if (print_warning) then
                  prerr_endline "Warning: force 3.402823466e+38 to 3.40282346639e+38 to pass the test"
              in
              "3.40282346639e+38"
            else if (se1 = "-1.175494351e-38" || se1 = "-1.17549e-38") then 
              let () = 
                if (print_warning) then
                  prerr_endline "Warning: force -1.175494351e-38 to -1.17549435082e-38 to pass the test"
              in
              "-1.17549435082e-38"
            else if (se1 = "-75618.93781" || se1 = "-75618.9") then 
              let () = 
                if (print_warning) then
                  prerr_endline "Warning: force -75618.93781 to -75618.9375 to pass the test"
              in
              "-75618.9375"
            else if (se1 = "-18.81") then 
              let () = 
                if (print_warning) then
                  prerr_endline "Warning: force -18.81 to -18.8099994659 to pass the test"
              in
              "-18.8099994659"
            else
              se1
          else
            se1
        in
        se1 = se2
      in
      if (not comparison) then (
        (* for float and double, try also a string comparison to avoid rounding problems *)
        match e1 with
        | Mp_data.Float _ -> comparison_float_double ()
        | Mp_data.Double _ -> comparison_float_double ()
        | Mp_data.Blob _ -> (
            let se2 = data_to_string e2 in
            if (se2 = "NULL") then (
              let () = 
                if (print_warning) then
                  prerr_endline "Warning: blob value is NULL. Please check that the LOAD_FILE() MySQL function can be used to insert files from the test directory. You may have to copy the binary files from this test directory to /tmp directory and to change the owner for those files with the MySQL user. If you have to make those changes, don't forget to modify the testFixture* code to set the testfile1 to testfile4 values to the new path of binary files."
              in
              (* force true to pass the test *)
              true
            )
            else
              comparison
          )
        | _ -> comparison
      )
      else (
        comparison
      )
    in
    let () = 
      let blob_binary_varbinary () =
        if (not comparison) then (
          let () = prerr_endline (Printf.sprintf "Failure in binary data comparison \n") in
          let () = prerr_endline (Printf.sprintf "MD5 - e1: %s" (Digest.to_hex (Digest.string (data_to_string e1)))) in
          let () = prerr_endline (Printf.sprintf "MD5 - e2: %s" (Digest.to_hex (Digest.string (data_to_string e2)))) in
          let () = prerr_endline (Printf.sprintf "Type - e1:\n%s" (Mp_data.type_to_string e1)) in
          let () = prerr_endline (Printf.sprintf "Type - e2:\n%s" (Mp_data.type_to_string e2)) in
          let () = prerr_endline (Printf.sprintf "Value - e1:\n%s\nLength - e1: %u" (data_to_string e1) (String.length (data_to_string e1))) in
          let () = prerr_endline (Printf.sprintf "Value - e2:\n%s\nLength - e2: %u" (data_to_string e2) (String.length (data_to_string e2))) in
          let () = flush stderr in
          ()
        )
      in
      match e1 with
      | Mp_data.Blob _ -> blob_binary_varbinary ()
      | Mp_data.Binary _ -> blob_binary_varbinary ()
      | Mp_data.Varbinary _ -> blob_binary_varbinary ()
      | _ -> (
          if (not comparison) then (
            let () = prerr_endline (Printf.sprintf "Failure in data comparison \n") in
            let () = prerr_endline (Printf.sprintf "Value e1: \n%s\nValue e2:\n%s\n" (data_to_string e1) (data_to_string e2) ) in
            let () = prerr_endline (Printf.sprintf "Length e1: %u  Length e2: %u\n" (String.length (data_to_string e1)) (String.length (data_to_string e2)) ) in
            let () = prerr_endline (Printf.sprintf "Type e1: %s  Type e2: %s\n" (Mp_data.type_to_string e1) (Mp_data.type_to_string e2) ) in
            let () = flush stderr in
            ()
          )
        )
    in
    acc && comparison
  in
  let f_list acc l1 l2 =
    acc && (List.fold_left2 f true l1 l2)
  in
  List.fold_left2 f_list true d1 d2

let records_equals r1 r2 = 
  let (fields_r1, data_r1) = r1 in 
  let (fields_r2, data_r2) = r2 in 
  (fields_equals fields_r1 fields_r2) && (data_equals data_r1 data_r2)

let test1 vendor connection records db_name version = 
  let () = 
    (* force the same timezone from fixture *)
    let sql = "SET time_zone='+0:0'" in
    let stmt = Mp_client.create_statement_from_string sql in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in

    let sql = "SELECT * FROM test_ocmp LIMIT 1" in
    let stmt = Mp_client.create_statement_from_string sql in
    let () = Mp_client.(
      assert_equal ~msg:sql 
        ~cmp:records_equals
        (fields, [List.nth records 0]) 
        (Test_query.try_query 
           ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
           ~sql:sql)
    ) in
    let f = 
      let r = Mp_client.(get_result_set(get_result(execute ~connection:connection ~statement:stmt ~return_all_raw_mysql_data:true ()))) in
      let r = r.Mp_result_set_packet.mysql_data in
      match r with
      | Some d -> (
          d.Mp_result_set_packet.result_set_field_packets
        )
      | None -> assert false
    in
    let fields_ok = mysql_fields vendor db_name connection.Mp_client.configuration.Mp_client.charset_number version in
    assert_equal ~msg:sql fields_ok (Test_query.try_query ~f:f ~sql:sql)
  in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( (fields, records) ) 
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_string_null_no_def FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( [("f_string_null_no_def", 0)], [[List.nth (List.nth records 0) 11]; [List.nth (List.nth records 1) 11]] )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_string_null_no_def, f_int_default_null FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( [("f_string_null_no_def", 0); ("f_int_default_null", 1)], 
        [
          [List.nth (List.nth records 0) 11; List.nth (List.nth records 0) 21];
          [List.nth (List.nth records 1) 11; List.nth (List.nth records 1) 21];
        ] )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_int_default_null FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( [("f_int_default_null", 0)], [[List.nth (List.nth records 0) 21]; [List.nth (List.nth records 1) 21]] )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_autoinc_not_null_no_def AS field1, f_int_null_no_def AS field2, f_smallint_null_no_def AS field3, f_decimal_12_4_null_no_def AS field4, f_datetime_null_no_def AS field5, f_float_null_no_def AS field6, f_double_null_no_def AS field7, f_int24_null_no_def AS field8, f_date_null_no_def AS field9, f_time_null_no_def AS field10, f_year_null_no_def AS field11, f_string_null_no_def AS field12, f_varstring_null_no_def AS field13, f_blobtext_null_no_def AS field14, f_blobblob_null_no_def AS field15, f_blobtiny_null_no_def AS field16, f_blobmedium_null_no_def AS field17, f_bloblong_null_no_def AS field18, f_blobimg_null_no_def AS field19, f_enum_null_no_def AS field20, f_set_null_no_def AS field21, f_int_default_null AS field22, f_timestamp_null_no_def AS field23, f_bit_null_no_def AS field24, f_tinyint_null_no_def_signed AS field25, f_tinyint_null_no_def_unsigned AS field26, f_int_null_no_def_signed AS field27, f_int_null_no_def_unsigned AS field28, f_smallint_null_no_def_signed AS field29, f_smallint_null_no_def_unsigned AS field30, f_decimal_65_20_null_no_def_signed AS field31, f_decimal_65_20_null_no_def_unsigned AS field32, f_float_null_no_def_signed AS field33, f_float_null_no_def_unsigned AS field34, f_double_null_no_def_signed AS field35, f_double_null_no_def_unsigned AS field36, f_int24_null_no_def_signed AS field37, f_int24_null_no_def_unsigned AS field38, f_bigint_null_no_def_signed AS field39, f_bigint_null_no_def_unsigned AS field40, f_string_default_null AS field41, f_date_not_null_def20110101 AS field42, f_datetime_not_null_def20111011140534 AS field43, f_timestamp_not_null_def20110510 AS field44, f_time_not_null_def214702 AS field45, f_big_enum_default_null AS field46, f_binary_default_null AS field47, f_varbinary_default_null AS field48, f_bigint_def_0 AS field49, f_int_def_0 AS field50, f_smallint_def_0 AS field51, f_decimal_20_9_def_0 AS field52, f_float_def_0 AS field53, f_double_def_0 AS field54, f_int24_def_0 AS field55, f_tinyint_def_0 AS field56 FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("field1", 0); ("field2", 1); ("field3", 2); ("field4", 3); ("field5", 4); ("field6", 5); ("field7", 6); ("field8", 7); ("field9", 8); ("field10", 9); ("field11", 10); ("field12", 11); ("field13", 12); ("field14", 13); ("field15", 14); ("field16", 15); ("field17", 16); ("field18", 17); ("field19", 18); ("field20", 19); ("field21", 20); ("field22", 21); ("field23", 22); ("field24", 23); ("field25", 24); ("field26", 25); ("field27", 26); ("field28", 27); ("field29", 28); ("field30", 29); ("field31", 30); ("field32", 31); ("field33", 32); ("field34", 33); ("field35", 34); ("field36", 35); ("field37", 36); ("field38", 37); ("field39", 38); ("field40", 39); ("field41", 40); ("field42", 41); ("field43", 42); ("field44", 43); ("field45", 44); ("field46", 45); ("field47", 46); ("field48", 47)   ; ("field49", 48); ("field50", 49); ("field51", 50); ("field52", 51); ("field53", 52); ("field54", 53); ("field55", 54); ("field56", 55)], records) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  ()

let test_geo connection records_geo = 
  let () = Mp_client.(
    let sql = "SELECT AsText(f_geo_point_null_no_def) AS field_geo_1 FROM test_ocmp_geo" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("field_geo_1", 0)], records_geo) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  ()

let test_blobbig connection records_blobbig = 
  let () = Mp_client.(
    let sql = "SELECT f_blobbig_null_no_def FROM test_ocmp_blobbig" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_blobbig_null_no_def", 0)], records_blobbig) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  ()

let test_date vendor connection records_date version = 
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_date" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_date_null_no_def", 0); 
          ("f_time_null_no_def", 1); 
          ("f_datetime_null_no_def", 2); 
          ("f_timestamp_null_no_def", 3)], records_date) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT DATE_FORMAT(f_datetime_null_no_def, \"%H:%i:%s.%f\") AS f1 FROM test_ocmp_date" in
    let stmt = create_statement_from_string sql in
    let df2 =
      match vendor with
      | Test_types.MySQL -> (
          if version >= 5611 then
            [Mp_data.data_varstring "13:17:05.000000"]
          else
            [Mp_data.data_varstring "13:17:04.000000"]
        )
      | Test_types.MariaDB -> (
          [Mp_data.data_varstring "13:17:04.000000"]
        )
    in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f1", 0)], [[Mp_data.data_varstring "00:00:00.000000"]; df2]) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT TIME('01:01:01.9999') AS f1, TIMESTAMP('2011-11-06 1:10:2.78951') AS f2, DATE('2010-5-28 14:1:16.1516') AS f3, f_date_null_no_def AS f4, TIME('-838:59:59') AS f5 FROM test_ocmp_date" in
    let stmt = create_statement_from_string sql in
    let v9999 = 
      if version >= 5611 then
        9999
      else
        999900
    in
    let v78951 = 
      if version >= 5611 then
        78951
      else
        789510
    in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f1", 0); ("f2", 1); ("f3", 2); ("f4", 3); ("f5", 4)], 
         [
           [Mp_data.data_time (Mp_data.Positive, 1, 1, 1, (Int64.of_int v9999));
            Mp_data.data_datetime ((2011, 11, 6), (1, 10, 2, (Int64.of_int v78951)));
            Mp_data.data_date (2010, 5, 28);
            Mp_data.data_date (0, 0, 0);
            Mp_data.data_time (Mp_data.Negative, 838, 59, 59, Int64.zero)];
           [Mp_data.data_time (Mp_data.Positive, 1, 1, 1, (Int64.of_int v9999));
            Mp_data.data_datetime ((2011, 11, 6), (1, 10, 2, (Int64.of_int v78951)));
            Mp_data.data_date (2010, 5, 28);
            Mp_data.data_date (1992, 12, 31);
            Mp_data.data_time (Mp_data.Negative, 838, 59, 59, Int64.zero)];
         ]) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  ()

let test_bigstring connection records_bigstring records_bigvarchar records_bigvarbinary = 
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_bigstring" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_bigstring_char255_def_null", 0); 
          ("f_bigstring_binary255_def_null", 1);], records_bigstring) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_bigvarchar" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_bigstring_varchar65532_def_empty", 0)], records_bigvarchar) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_bigvarbinary" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_bigstring_varbinary65532_def_null", 0)], records_bigvarbinary) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
         ~sql:sql)
  ) in
  ()

let test_manyblobs connection records_manyblobs db_name = 
  let configuration = connection.Mp_client.configuration in
  let configuration = { configuration with Mp_client.databasename = db_name } in
  let connection = { connection with Mp_client.configuration = configuration } in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_manyblobs" in
    let stmt = create_statement_from_string sql in
    if (Sys.word_size = 64) then
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( ([("f_blob1_def_null", 0);
            ("f_blob2_def_null", 1);
            ("f_blob3_def_null", 2);
            ("f_blob4_def_null", 3);
            ("f_blob5_def_null", 4);
            ("f_blob6_def_null", 5);
            ("f_blob7_def_null", 6);
            ("f_blob8_def_null", 7);
            ("f_blob9_def_null", 8);
            ("f_blob10_def_null", 9);
           ], records_manyblobs) )
        (Test_query.try_query 
           ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ()))).Mp_result_set_packet.rows ) 
           ~sql:sql)
  ) in
  ()  

let test_filter_iter connection records ok_value_iter = 
  let () = 
    let sql = "SELECT * FROM test_ocmp" in
    let stmt = Mp_client.create_statement_from_string sql in
    let filter fields row = 
      let d = List.nth row (List.assoc "f_date_null_no_def" fields) in
      match d with
      | Mp_data.Date v -> (
          let (year, _, _) = v in
          if (year >= 2000) then true else false
        )
      | _ -> assert false
    in
    let acc = ref "" in
    let iter acc fields row = 
      let d = List.nth row (List.assoc "f_date_null_no_def" fields) in
      let s = List.nth row (List.assoc "f_varstring_null_no_def" fields) in
      acc := (data_to_string d) ^ (data_to_string s)
    in
    let () = Mp_client.(
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 0]) )
        (Test_query.try_query 
           ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:stmt ~filter:(Some filter) ~iter:(Some (iter acc)) ()))).Mp_result_set_packet.rows) ~sql:sql)
    ) in
    assert_equal ~msg:sql 
      ok_value_iter !acc
  in
  ()

let test_proc_one_result connection records_proc_one_result =
  let first e = 
    match e with
    | first::_ ->
        let x = Mp_client.get_result_set first in
        x.Mp_result_set_packet.rows
    | _ -> assert false
  in
  let () = 
    let sql = "CALL test_ocmp_proc_one_result" in
    let stmt = Mp_client.create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ([("oh", 0); ("abc", 1)], records_proc_one_result)
      (Test_query.try_query
         ~f:(first(Mp_client.get_result_multiple(Mp_client.execute ~connection:connection ~statement:stmt ())))
         ~sql:sql)
  in
  ()

let test_proc_multiple_results connection records_proc_multiple_results = 
  let () = 
    let cmp ok test = 
      (records_equals (List.nth ok 0) (List.nth test 0))
      && (records_equals (List.nth ok 1) (List.nth test 1))
      && (records_equals (List.nth ok 2) (List.nth test 2))
    in
    let all e = 
      match e with
      | first::second::third::_ -> (
          let f r =
            let x = Mp_client.get_result_set r in
            x.Mp_result_set_packet.rows
          in
          [f first; f second; f third]
        )
      | _ -> assert false
    in
    let sql = "CALL test_ocmp_proc_multiple_results" in
    let stmt = Mp_client.create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:cmp
      ([
        ([("one", 0); ("a", 1)], [List.nth records_proc_multiple_results 0]);
        ([("two", 0); ("b", 1)], [List.nth records_proc_multiple_results 1]);
        ([("three", 0); ("c", 1)], [List.nth records_proc_multiple_results 2])
      ])
      (Test_query.try_query 
         ~f:(all(Mp_client.get_result_multiple(Mp_client.execute ~connection:connection ~statement:stmt ()))) 
         ~sql:sql)
  in
  ()

let test host connection encoding _ = 
  let (vendor, version, _, _, _) = host in
  (*
    We use AsText() function to retrieve the geometry data
    so it's a blob type and not a geometry one
  *)
  let blobgeo1 = Buffer.create 128 in
  let blobgeo2 = Buffer.create 128 in
  let records_geo = 
    if (version <= 5095) then (
      [ [Mp_data.data_varbinary (Buffer.add_string blobgeo1 "POINT(231 4)"; blobgeo1); ];
        [Mp_data.data_varbinary (Buffer.add_string blobgeo2 "POINT(-2 5)"; blobgeo2); ] ]
    )
    else (
      [ [Mp_data.data_blob (Buffer.add_string blobgeo1 "POINT(231 4)"; blobgeo1); ];
        [Mp_data.data_blob (Buffer.add_string blobgeo2 "POINT(-2 5)"; blobgeo2); ] ]
    ) 
  in
  let module F = (
    val (
      match encoding with
      | Mp_charset.Latin1 -> (
          let module E = struct
            include Fixture_latin1
          end
          in (module E : Fixture.FIXTURE)
        )
      | Mp_charset.Utf8 -> (
          let module E = struct
            include Fixture_utf8
          end
          in (module E : Fixture.FIXTURE)
        )
      | _ -> assert false
    ) : Fixture.FIXTURE
  )
  in
  let () = test1 vendor connection F.records F.db_name version in
  let () = test_geo connection records_geo in
  let () = test_blobbig connection F.records_blobbig in
  let () = test_date vendor connection (F.records_date vendor version) version in
  let () = test_bigstring connection F.records_bigstring F.records_bigvarchar F.records_bigvarbinary in
  let () = test_manyblobs connection F.records_manyblobs F.db_name in
  let () = test_proc_one_result connection (F.records_proc_one_result vendor) in
  let () = test_proc_multiple_results connection (F.records_proc_multiple_results vendor) in
  let () = test_filter_iter connection F.records F.ok_value_iter in
  ()
