open OUnit
open Mysql_protocol

let fields = Test_query_select.fields
let records_equals = Test_query_select.records_equals

let test1 connection records = 
  let () = 
    let sql = "SELECT f_string_null_no_def FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( ([("f_string_null_no_def", 0)], [[List.nth (List.nth records 0) 11]]) )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( [("f_string_null_no_def", 0)], [[List.nth (List.nth records 1) 11]] )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT f_string_null_no_def, f_int_default_null FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( [("f_string_null_no_def", 0); ("f_int_default_null", 1)], 
          [
            [List.nth (List.nth records 0) 11; List.nth (List.nth records 0) 21];
          ] )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( [("f_string_null_no_def", 0); ("f_int_default_null", 1)], 
          [
            [List.nth (List.nth records 1) 11; List.nth (List.nth records 1) 21];
          ] )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT f_int_default_null FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( [("f_int_default_null", 0)], [[List.nth (List.nth records 0) 21]] )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( [("f_int_default_null", 0)], [[List.nth (List.nth records 1) 21]] )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT * FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 0]) ) 
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 1]) ) 
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT f_string_null_no_def FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( ([("f_string_null_no_def", 0)], [[List.nth (List.nth records 0) 11]; [List.nth (List.nth records 1) 11]]) )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT f_string_null_no_def, f_int_default_null FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( [("f_string_null_no_def", 0); ("f_int_default_null", 1)], 
          [[List.nth (List.nth records 0) 11; List.nth (List.nth records 0) 21]; 
           [List.nth (List.nth records 1) 11; List.nth (List.nth records 1) 21]] )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT f_int_default_null FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( [("f_int_default_null", 0)], [[List.nth (List.nth records 0) 21]; [List.nth (List.nth records 1) 21]] )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT * FROM test_ocmp WHERE f_autoinc_not_null_no_def > ?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 0)] in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 0; List.nth records 1]) ) 
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
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
      [ List.nth (List.nth records i) 0;
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
    let params = get_params 0 in
    let p = Mp_client.execute ~connection:connection ~statement:p ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 0]) ) 
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  ()

let test_bigstring connection records_bigstring records_bigvarchar records_bigvarbinary = 
  let () = 
    let sql = "SELECT * FROM test_ocmp_bigstring" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let p = Mp_client.execute ~connection:connection ~statement:p ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( ([("f_bigstring_char255_def_null", 0); 
            ("f_bigstring_binary255_def_null", 1);], records_bigstring) )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT * FROM test_ocmp_bigvarchar" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let p = Mp_client.execute ~connection:connection ~statement:p ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( ([("f_bigstring_varchar65532_def_empty", 0)], records_bigvarchar) ) 
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  let () = 
    let sql = "SELECT * FROM test_ocmp_bigvarbinary" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
    let p = Mp_client.execute ~connection:connection ~statement:p ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( ([("f_bigstring_varbinary65532_def_null", 0)], records_bigvarbinary) )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  ()

let test_manyblobs connection records_manyblobs db_name = 
  let configuration = connection.Mp_client.configuration in
  let configuration = { configuration with Mp_client.databasename = db_name } in
  let connection = { connection with Mp_client.configuration = configuration } in
  let () = 
    let sql = "SELECT * FROM test_ocmp_manyblobs" in
    let stmt = Mp_client.create_statement_from_string sql in
    if (Sys.word_size = 64) then
      let p = Mp_client.prepare ~connection:connection ~statement:stmt in
      let p = Mp_client.execute ~connection:connection ~statement:p ~flag:Mp_execute.Cursor_type_read_only () in
      let () = 
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
             ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
             ~sql:sql)
      in
      let () = 
        assert_raises ~msg:sql 
          Mp_client.Fetch_no_more_rows 
          ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
      in
      ()
    else
      ()
  in
  ()  

let data_to_string d =
  let s = Mp_data.to_string d in
  match s with
  | None -> "NULL"
  | Some s -> s

let test_filter_iter connection records ok_value_iter = 
  let () = 
    let sql = "SELECT * FROM test_ocmp" in
    let stmt = Mp_client.create_statement_from_string sql in
    let p = Mp_client.prepare ~connection:connection ~statement:stmt in
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
    let p = Mp_client.execute ~connection:connection ~statement:p ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      assert_equal ~msg:sql 
        ~cmp:records_equals
        ( (fields, [List.nth records 0]) )
        (Test_query.try_query 
           ~f:( (Mp_client.get_fetch_result_set(Mp_client.fetch ~connection:connection ~statement:p 
                                                  ~filter:(Some filter) ~iter:(Some (iter acc)) ~nb_rows:(Int64.of_int 2) ())).Mp_result_set_packet.rows ) 
           ~sql:sql)
    in
    let () = 
      assert_equal ~msg:sql 
        ok_value_iter !acc
    in
    let () = 
      assert_raises ~msg:sql 
        Mp_client.Fetch_no_more_rows 
        ((fun _ -> Mp_client.fetch ~connection:connection ~statement:p ()))
    in
    ()
  in
  ()

let test connection encoding _ = 
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
    let () = test_bigstring connection F.records_bigstring F.records_bigvarchar F.records_bigvarbinary in
    let () = test_manyblobs connection F.records_manyblobs F.db_name in
    let () = test_filter_iter connection F.records F.ok_value_iter in
    ()
  with
  | Mp_client.Error err as e -> (
      let () = prerr_endline (Mp_client.error_exception_to_string err) in
      raise e
    )
