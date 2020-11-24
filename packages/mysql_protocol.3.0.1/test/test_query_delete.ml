open OUnit
open Mysql_protocol

let build_ok_delete affected status = 
  { Mp_client.affected_rows = affected;
    Mp_client.insert_id = (Int64.zero, Big_int.zero_big_int);
    Mp_client.server_status = status;
    Mp_client.warning_count = 0;
    Mp_client.message = "";
  }

let result_equals ok1 r =
  let ok2 = build_ok_delete ok1.Mp_client.affected_rows 8704 in

  let r_eq ok r =
    let affected_rows_ok = ok.Mp_client.affected_rows in
    let (insert_id_int64_ok, insert_id_big_int_ok) = ok.Mp_client.insert_id in
    let server_status_ok = ok.Mp_client.server_status in
    let warning_count_ok = ok.Mp_client.warning_count in
    let message_ok = ok.Mp_client.message in

    let affected_rows_r = r.Mp_client.affected_rows in
    let (insert_id_int64_r, insert_id_big_int_r) = r.Mp_client.insert_id in
    let server_status_r = r.Mp_client.server_status in
    let warning_count_r = r.Mp_client.warning_count in
    let message_r = r.Mp_client.message in

    (affected_rows_ok = affected_rows_r)
    && (Int64.compare insert_id_int64_ok insert_id_int64_r = 0)
    && (Big_int.compare_big_int insert_id_big_int_ok insert_id_big_int_r = 0)
    && (server_status_ok = server_status_r)
    && (warning_count_ok = warning_count_r)
    && (message_ok = message_r)
  in

  if (r_eq ok1 r || r_eq ok2 r) then
    true
  else
    false

let test1 connection =
  let () = Mp_client.(
    let sql = "DELETE FROM test_ocmp WHERE f_autoinc_not_null_no_def=4" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:result_equals
      (build_ok_delete (Int64.of_int 1) 512)
      (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "DELETE FROM test_ocmp WHERE f_string_null_no_def='string : ABCDEFGHIJKLMNOPQRSTUVWXYZ'" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:result_equals
      (build_ok_delete (Int64.of_int 1) 512)
      (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "DELETE FROM test_ocmp WHERE f_int24_null_no_def=1677721" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:result_equals
      (build_ok_delete (Int64.of_int 1) 512)
      (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "DELETE FROM test_ocmp WHERE f_string_null_no_def='not exist : XXXXXXXXXXXXXXXXXXXXX'" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:result_equals
      (build_ok_delete (Int64.of_int 0) 512)
      (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
  ) in
  let () = Mp_client.(
    let sql = "DELETE FROM test_ocmp" in
    let stmt = create_statement_from_string sql in
    assert_equal ~msg:sql 
      ~cmp:result_equals
      (* status is either 512 or 8704, it seems to be not the same depending on the MySQL version *)
      (build_ok_delete (Int64.of_int 5) 512)
      (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
  ) in
  ()

let test connection _ = 
  try
    test1 connection 
  with
  | Mp_client.Error e -> prerr_endline (Mp_client.error_exception_to_string e)
