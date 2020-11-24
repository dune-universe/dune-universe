open OUnit
open Mysql_protocol

let fields = Test_query_select.fields
let records_equals = Test_query_select.records_equals

let test1 connection records = 
  let () = Mp_client.(
    let sql = "SELECT f_binary_default_null FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( [("f_binary_default_null", 0)], [[List.nth (List.nth records 0) 46]; [List.nth (List.nth records 1) 46]] )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_string_null_no_def FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( [("f_string_null_no_def", 0)], [[List.nth (List.nth records 0) 11]; [List.nth (List.nth records 1) 11]] )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_string_null_no_def, f_int_default_null FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( [("f_string_null_no_def", 0); ("f_int_default_null", 1)], 
        [
          [List.nth (List.nth records 0) 11; List.nth (List.nth records 0) 21];
          [List.nth (List.nth records 1) 11; List.nth (List.nth records 1) 21];
        ] )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_int_default_null FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( [("f_int_default_null", 0)], [[List.nth (List.nth records 0) 21]; [List.nth (List.nth records 1) 21]] )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( (fields, records) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp WHERE f_int_default_null = ?" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_null] in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( (fields, []) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ~params:params ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in

  let sql = {|
      SELECT * FROM test_ocmp WHERE
            f_autoinc_not_null_no_def = ?
        AND f_int_null_no_def = ?
        AND f_smallint_null_no_def = ?
        AND f_decimal_12_4_null_no_def = ?
        AND f_datetime_null_no_def = ?
        AND f_float_null_no_def = ?
        AND f_double_null_no_def = ?
        AND f_int24_null_no_def = ?
        AND f_date_null_no_def = ?
        AND f_time_null_no_def = ?
        AND f_year_null_no_def = ?
        AND f_string_null_no_def = ?
        AND f_varstring_null_no_def = ?
        /* f_blobtext_null_no_def TEXT */
        AND f_blobblob_null_no_def = ?
        AND f_blobtiny_null_no_def = ?
        AND f_blobmedium_null_no_def = ?
        AND f_bloblong_null_no_def = ?
        /* f_blobimg_null_no_def BLOB */
        AND f_enum_null_no_def = ?
        AND (f_set_null_no_def = ?
          OR f_int_default_null IN (?))
        AND f_timestamp_null_no_def = ?
        AND f_bit_null_no_def = ?
        AND f_tinyint_null_no_def_signed = ?
        AND f_tinyint_null_no_def_unsigned = ?
        AND f_int_null_no_def_signed = ?
        AND f_int_null_no_def_unsigned = ?
        AND f_smallint_null_no_def_signed = ?
        AND f_smallint_null_no_def_unsigned = ?
        AND f_decimal_65_20_null_no_def_signed = ?
        AND f_decimal_65_20_null_no_def_unsigned = ?
        AND f_float_null_no_def_signed - 1e-307 <= ?
        AND f_float_null_no_def_unsigned - 1e-307 <= ?
        AND f_double_null_no_def_signed - 1e-307 <= ?
        AND f_double_null_no_def_unsigned - 1e-307 <= ?
        AND f_int24_null_no_def_signed = ?
        AND f_int24_null_no_def_unsigned = ?
        AND f_bigint_null_no_def_signed = ?
        AND f_bigint_null_no_def_unsigned = ?
        AND (f_string_default_null = ?
          OR f_date_not_null_def20110101 = ?)
        AND f_datetime_not_null_def20111011140534 = ?
        AND f_timestamp_not_null_def20110510 = ?
        AND (f_time_not_null_def214702 = ?
          OR f_big_enum_default_null = ?)
        AND f_binary_default_null = ?
        AND f_varbinary_default_null = ?
        AND f_bigint_def_0 = ?
        AND f_int_def_0 = ?
        AND f_smallint_def_0 = ?
        AND f_decimal_20_9_def_0 = ?
        /* f_float_def_0 FLOAT */
        /* f_double_def_0 DOUBLE */
        AND f_int24_def_0 = ?
        AND f_tinyint_def_0 = ? |}
  in
  let stmt = Mp_client.create_statement_from_string sql in
  let p = Mp_client.prepare ~connection:connection ~statement:stmt in
  let get_params i = 
    [   List.nth (List.nth records i) 0;
        List.nth (List.nth records i) 1;
        List.nth (List.nth records i) 2;
        List.nth (List.nth records i) 3;
        List.nth (List.nth records i) 4;
        List.nth (List.nth records i) 5;
        List.nth (List.nth records i) 6;
        List.nth (List.nth records i) 7;
        List.nth (List.nth records i) 8;
        List.nth (List.nth records i) 9;
        List.nth (List.nth records i) 10;
        List.nth (List.nth records i) 11;
        List.nth (List.nth records i) 12;
        (* List.nth (List.nth records i) 13; (* f_blobtext_null_no_def *) query returns empty result with MariaDB 10.5.6 *)
        List.nth (List.nth records i) 14;
        List.nth (List.nth records i) 15;
        List.nth (List.nth records i) 16;
        List.nth (List.nth records i) 17;
        (* List.nth (List.nth records i) 18; (* f_blobimg_null_no_def *) *)
        List.nth (List.nth records i) 19;
        List.nth (List.nth records i) 20;
        List.nth (List.nth records i) 21;
        List.nth (List.nth records i) 22;
        List.nth (List.nth records i) 23;
        List.nth (List.nth records i) 24; 
        List.nth (List.nth records i) 25;
        List.nth (List.nth records i) 26;
        List.nth (List.nth records i) 27;
        List.nth (List.nth records i) 28;
        List.nth (List.nth records i) 29;
        List.nth (List.nth records i) 30;
        List.nth (List.nth records i) 31;
        List.nth (List.nth records i) 32;
        List.nth (List.nth records i) 33;
        List.nth (List.nth records i) 34;
        List.nth (List.nth records i) 35;
        List.nth (List.nth records i) 36; 
        List.nth (List.nth records i) 37;
        List.nth (List.nth records i) 38;
        List.nth (List.nth records i) 39; 
        List.nth (List.nth records i) 40;
        List.nth (List.nth records i) 41;
        List.nth (List.nth records i) 42; 
        List.nth (List.nth records i) 43;
        List.nth (List.nth records i) 44;
        List.nth (List.nth records i) 45; 
        List.nth (List.nth records i) 46;
        List.nth (List.nth records i) 47;
        List.nth (List.nth records i) 48; 
        List.nth (List.nth records i) 49;
        List.nth (List.nth records i) 50;
        List.nth (List.nth records i) 51; 
        (* List.nth (List.nth records i) 52; (* f_float_def_0  *) query returns empty result with MySQL 5.1.x *)
        (* List.nth (List.nth records i) 53; (* f_double_def_0 *) query returns empty result with MySQL 5.1.x *)
        List.nth (List.nth records i) 54;
        List.nth (List.nth records i) 55; 
      ]
  in
  let () = Mp_client.(
    let params = get_params 0 in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 0]) )
        (Test_query.try_query 
           ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ~params:params ()))).Mp_result_set_packet.rows )
           ~sql:sql)
    in
    let () = 
      let params = get_params 1 in
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 1]) )
        (Test_query.try_query 
           ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ~params:params ()))).Mp_result_set_packet.rows )
           ~sql:sql)
    in
    let () = 
      (* same execute but without rebinding *)
      let params = get_params 1 in
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 1]) )
        (Test_query.try_query 
           ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ~params:params ~bind:Mp_execute.No_bind ()))).Mp_result_set_packet.rows )
           ~sql:sql)
    in
    ()
  ) in

  (* special tests for date/time *)
  let () = Mp_client.(
    let sql = "SELECT TIME('01:01:01.9999') AS f1, TIMESTAMP('2011-11-06 1:10:2.78951') AS f2, f_date_null_no_def AS f3, TIME('-736:41:21.99') AS f4, TIME('-51.34') AS f5, TIME('0:0:0') AS f6 FROM test_ocmp_date LIMIT 1" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f1", 0); ("f2", 1); ("f3", 2); ("f4", 3); ("f5", 4); ("f6", 5)], 
         [
           [Mp_data.data_time (Mp_data.Positive, 1, 1, 1, (Int64.of_int 999900));
            Mp_data.data_datetime ((2011, 11, 6), (1, 10, 2, (Int64.of_int 789510)));
            Mp_data.data_date (0, 0, 0);
            Mp_data.data_time (Mp_data.Negative, 736, 41, 21, (Int64.of_int 990000));
            Mp_data.data_time (Mp_data.Negative, 0, 0, 51, (Int64.of_int 340000));
            Mp_data.data_time (Mp_data.Positive, 0, 0, 0, Int64.zero)];
         ]) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT f_varstring_null_no_def FROM test_ocmp WHERE f_autoinc_not_null_no_def = ? OR f_string_null_no_def = ? LIMIT 1" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    let params = [List.nth (List.nth records 0) 0; Mp_data.data_string "...special ' characters \" ..."] in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_varstring_null_no_def", 0)], [[List.nth (List.nth records 0) 12]]) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ~params:params ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  ()

let test_geo connection records_geo = 
  let () = Mp_client.(
    let sql = "SELECT AsText(f_geo_point_null_no_def) AS field_geo_1 FROM test_ocmp_geo" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("field_geo_1", 0)], records_geo) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  ()

let test_blobbig connection records_blobbig = 
  let () = Mp_client.(
    let sql = "SELECT f_blobbig_null_no_def FROM test_ocmp_blobbig" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_blobbig_null_no_def", 0)], records_blobbig) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  ()

let test_date vendor connection records_date version = 
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_date" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_date_null_no_def", 0); 
          ("f_time_null_no_def", 1); 
          ("f_datetime_null_no_def", 2); 
          ("f_timestamp_null_no_def", 3)], records_date) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT DATE_FORMAT(f_datetime_null_no_def, \"%H:%i:%s.%f\") AS f1 FROM test_ocmp_date" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    let df2 =
      match vendor with
      | Test_types.MySQL ->
          if version >= 5611 then
            [Mp_data.data_varstring "13:17:05.000000"]
          else
            [Mp_data.data_varstring "13:17:04.000000"]
      | Test_types.MariaDB ->
          [Mp_data.data_varstring "13:17:04.000000"]
    in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f1", 0)], [[Mp_data.data_varstring "00:00:00.000000"]; df2]) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT TIME('01:01:01.9999') AS f1, TIMESTAMP('2011-11-06 1:10:2.78951') AS f2, DATE('2010-5-28 14:1:16.1516') AS f3, f_date_null_no_def AS f4, TIME('-838:59:59') AS f5 FROM test_ocmp_date" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f1", 0); ("f2", 1); ("f3", 2); ("f4", 3); ("f5", 4)], 
         [
           [Mp_data.data_time (Mp_data.Positive, 1, 1, 1, (Int64.of_int 999900));
            Mp_data.data_datetime ((2011, 11, 6), (1, 10, 2, (Int64.of_int 789510)));
            Mp_data.data_date (2010, 5, 28);
            Mp_data.data_date (0, 0, 0);
            Mp_data.data_time (Mp_data.Negative, 838, 59, 59, Int64.zero)];
           [Mp_data.data_time (Mp_data.Positive, 1, 1, 1, (Int64.of_int 999900));
            Mp_data.data_datetime ((2011, 11, 6), (1, 10, 2, (Int64.of_int 789510)));
            Mp_data.data_date (2010, 5, 28);
            Mp_data.data_date (1992, 12, 31);
            Mp_data.data_time (Mp_data.Negative, 838, 59, 59, Int64.zero)];
         ]) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  ()

let test_bigstring connection records_bigstring records_bigvarchar records_bigvarbinary = 
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_bigstring" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_bigstring_char255_def_null", 0); 
          ("f_bigstring_binary255_def_null", 1);], records_bigstring) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_bigvarchar" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_bigstring_varchar65532_def_empty", 0)], records_bigvarchar) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp_bigvarbinary" in
    let stmt = create_statement_from_string sql in
    let p = prepare ~connection:connection ~statement:stmt in
    assert_equal ~msg:sql 
      ~cmp:records_equals
      ( ([("f_bigstring_varbinary65532_def_null", 0)], records_bigvarbinary) )
      (Test_query.try_query 
         ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ()))).Mp_result_set_packet.rows )
         ~sql:sql)
  ) in
  ()

let data_to_string d =
  let s = Mp_data.to_string d in
  match s with
  | None -> "NULL"
  | Some s -> s

let test_filter_iter connection records ok_value_iter = 
  let () = Mp_client.(
    let sql = "SELECT * FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
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
    let () = 
      let p = prepare ~connection:connection ~statement:stmt in
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 0]) )
        (Test_query.try_query 
           ~f:( (get_result_set(get_result(execute ~connection:connection ~statement:p ~filter:(Some filter) ~iter:(Some (iter acc)) ()))).Mp_result_set_packet.rows )
           ~sql:sql)
    in
    assert_equal ~msg:sql 
      ok_value_iter !acc
  ) in
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
  try
    let () = test1 connection F.records in
    let () = test_geo connection records_geo in
    let () = test_blobbig connection F.records_blobbig in
    let () = test_date vendor connection (F.records_date vendor version) version in
    let () = test_bigstring connection F.records_bigstring F.records_bigvarchar F.records_bigvarbinary in
    let () = test_filter_iter connection F.records F.ok_value_iter in
    ()
  with
  | Mp_client.Error err as e -> (
      let () = prerr_endline (Mp_client.error_exception_to_string err) in
      raise e
    )
