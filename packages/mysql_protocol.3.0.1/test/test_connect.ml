open OUnit
open Mysql_protocol

let build_ok_update affected matched changed = 
  { Mp_client.affected_rows = Int64.of_int affected;
    Mp_client.insert_id = (Int64.zero, Big_int.zero_big_int);
    Mp_client.server_status = 8704;
    Mp_client.warning_count = 0;
    Mp_client.message = "(Rows matched: " ^ (string_of_int matched) ^ "  Changed: " ^ (string_of_int changed) ^ "  Warnings: 0";
  }

let result_equals ok r =
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

let test1 host config charset = 
  let (_, _, connection_type, db_user, db_password) = host in
  let () = 
    (* configuration *)
    let sockaddr = match connection_type with
      | Test_types.CInet (_, addr, port) -> Unix.ADDR_INET(addr, port)
      | Test_types.CUnix path -> Unix.ADDR_UNIX path
    in
    let databasename = config.Mp_client.databasename in
    let config = Mp_client.configuration 
        ~user:db_user ~password:db_password ~sockaddr:sockaddr 
        ~databasename:databasename ~charset:charset () in
    let connection = Mp_client.connect ~configuration:config ~force:true () in
    let sql = "UPDATE test_ocmp SET f_int_null_no_def = 7 WHERE f_int_null_no_def > f_int_null_no_def + 1" in
    let stmt = Mp_client.create_statement_from_string sql in
    let () = Mp_client.(
      assert_equal ~msg:sql
        ~cmp:result_equals
        (build_ok_update 0 0 0)
        (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
    ) in
    let () = Mp_client.disconnect ~connection:connection in
    (* don't set the database name in the configuration and send a use_database : should be ok *)
    let config = 
      Mp_client.configuration ~user:db_user ~password:db_password ~sockaddr:sockaddr ~charset:charset () 
    in
    let connection = Mp_client.connect ~configuration:config ~force:true () in
    let () = Mp_client.use_database ~connection:connection ~databasename:databasename in
    let sql = "UPDATE test_ocmp SET f_int_null_no_def = 7 WHERE f_int_null_no_def > f_int_null_no_def + 1" in
    let stmt = Mp_client.create_statement_from_string sql in
    let () = Mp_client.(
      assert_equal ~msg:sql
        ~cmp:result_equals
        (build_ok_update 0 0 0)
        (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
    ) in
    let () = Mp_client.disconnect ~connection:connection in
    (* don't set the database name in the configuration and don't send a use_database : should be ko *)
    let config = 
      Mp_client.configuration ~user:db_user ~password:db_password ~sockaddr:sockaddr ~charset:charset () 
    in
    let connection = Mp_client.connect ~configuration:config ~force:true () in
    let sql = "UPDATE test_ocmp SET f_int_null_no_def = 7 WHERE f_int_null_no_def > f_int_null_no_def + 1" in
    let stmt = Mp_client.create_statement_from_string sql in
    let () = Mp_client.(
      assert_raises ~msg:sql
        (Error {
            client_error_errno = 1046;
            client_error_sqlstate = "3D000";
            client_error_message = "No database selected"
          } ) 
        (fun _ -> (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql))
    ) in
    let () = Mp_client.disconnect ~connection:connection in
    (* test the mysql_native_password case with the special user *)
    let config = 
      Mp_client.configuration ~user:"u_ocmp_npauth" ~password:"ocmpnpauth"
        ~sockaddr:sockaddr ~charset:charset ~databasename:databasename () 
    in
    let connection = Mp_client.connect ~configuration:config ~force:true () in
    let sql = "UPDATE test_ocmp SET f_int_null_no_def = 7 WHERE f_int_null_no_def > f_int_null_no_def + 1" in
    let stmt = Mp_client.create_statement_from_string sql in
    let () = Mp_client.(
      assert_equal ~msg:sql
        ~cmp:result_equals
        (build_ok_update 0 0 0)
        (Test_query.try_query ~f:(get_result_ok(get_result(execute ~connection:connection ~statement:stmt ()))) ~sql:sql)
    ) in
    let () = Mp_client.disconnect ~connection:connection in
    ()
  in
  ()

let test host config charset _ = 
  try
    test1 host config charset
  with
  | Mp_client.Error err as e -> (
      let () = prerr_endline (Mp_client.error_exception_to_string err) in
      raise e
    )
