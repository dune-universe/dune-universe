open OUnit
open Mysql_protocol

let test1 connection =
  let () = Mp_client.(
    let id = "4294967290" in
    let sql = "TRUNCATE test_ocmp_auto_increment_ui" in
    let stmt = create_statement_from_string sql in
    let _ = execute ~connection:connection ~statement:stmt () in
    let sql = "ALTER TABLE test_ocmp_auto_increment_ui AUTO_INCREMENT=" ^ id in
    let stmt = create_statement_from_string sql in
    let _ = execute ~connection:connection ~statement:stmt () in
    let sql = "INSERT INTO test_ocmp_auto_increment_ui (f_int) VALUES (100)" in
    let stmt = create_statement_from_string sql in
    let (insert_id_int64, _) =
        (get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))).insert_id
    in
    let () = assert_equal ~msg:("INSERT_ID INT UNSIGNED: " ^ sql) (Int64.compare insert_id_int64 (Int64.of_string id)) 0 in

    let id = "18446744073709551610" in
    let sql = "TRUNCATE test_ocmp_auto_increment_ubi" in
    let stmt = create_statement_from_string sql in
    let _ = execute ~connection:connection ~statement:stmt () in
    let sql = "ALTER TABLE test_ocmp_auto_increment_ubi AUTO_INCREMENT=" ^ id in
    let stmt = create_statement_from_string sql in
    let _ = execute ~connection:connection ~statement:stmt () in
    let sql = "INSERT INTO test_ocmp_auto_increment_ubi (f_int) VALUES (200)" in
    let stmt = create_statement_from_string sql in
    let (_, insert_id_big_int) =
        (get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))).insert_id
    in
    let () = assert_equal ~msg:("INSERT_ID BIGINT UNSIGNED: " ^ sql)
        (Big_int.compare_big_int insert_id_big_int (Big_int.big_int_of_string id)) 0
    in

    let id = "-9223372036854775800" in
    let sql = "TRUNCATE test_ocmp_auto_increment_sbi" in
    let stmt = create_statement_from_string sql in
    let _ = execute ~connection:connection ~statement:stmt () in
    let sql = "INSERT INTO test_ocmp_auto_increment_sbi (f_autoinc, f_int) VALUES (" ^ id ^ ", 300)" in
    let stmt = create_statement_from_string sql in
    let (insert_id_int64, _) =
        (get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))).insert_id
    in
    let () = assert_equal ~msg:("INSERT_ID BIGINT SIGNED 1: " ^ sql) (Int64.compare insert_id_int64 (Int64.of_string id)) 0 in

    let id = "9223372036854775800" in
    let sql = "TRUNCATE test_ocmp_auto_increment_sbi" in
    let stmt = create_statement_from_string sql in
    let _ = execute ~connection:connection ~statement:stmt () in
    let sql = "ALTER TABLE test_ocmp_auto_increment_sbi AUTO_INCREMENT=" ^ id in
    let stmt = create_statement_from_string sql in
    let _ = execute ~connection:connection ~statement:stmt () in
    let sql = "INSERT INTO test_ocmp_auto_increment_sbi (f_int) VALUES (400)" in
    let stmt = create_statement_from_string sql in
    let (insert_id_int64, _) =
        (get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))).insert_id
    in
    let () = assert_equal ~msg:("INSERT_ID BIGINT SIGNED 2: " ^ sql) (Int64.compare insert_id_int64 (Int64.of_string id)) 0 in
    ()
  ) in
  ()

let test connection _ =
  try
    test1 connection
  with
  | Mp_client.Error e -> prerr_endline (Mp_client.error_exception_to_string e)
