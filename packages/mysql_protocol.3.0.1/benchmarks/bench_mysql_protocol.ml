
module Mp_client = Mysql_protocol.Mp_client;;
module Mp_data = Mysql_protocol.Mp_data;;
module Mp_execute = Mysql_protocol.Mp_execute;;
module Mp_result_set_packet = Mysql_protocol.Mp_result_set_packet;;

let print_result sql r = 
  print_endline ("Result of the SQL statement \"" ^ sql ^ "\": \n  " ^ (Mp_client.dml_dcl_result_to_string r) ^ "\n")

let print_row fields row = 
  let print_data f = 
    let (field_name, field_pos) = f in
    let data = List.nth row field_pos in
    print_endline ("  " ^ field_name ^ ": " ^ (Mp_data.to_string data))
  in
  let () = List.iter print_data fields in
  print_endline "  -- --  "

let print_set sql r = 
  let (fields, rows) = r.Mp_result_set_packet.rows in
  let () = print_endline ("Result set for the SQL statement \"" ^ sql ^ "\": \n") in
  let print_rows =   
    let () = List.iter (print_row fields) rows in
    print_newline ()
  in
  print_rows

let run() = 
  try
    (* let addr = Bench_config.addr in
    let port = Bench_config.port in *)
    let sockaddr = Bench_config.sockaddr in
    let db_user = Bench_config.db_user in
    let db_password = Bench_config.db_password in
    let db_name = Bench_config.db_name in

    let config = Mp_client.configuration  ~user:db_user ~password:db_password ~sockaddr:sockaddr ~databasename:db_name () in

    let connection = Mp_client.connect ~configuration:config () in

    let () = Mp_client.use_database ~connection:connection ~databasename:db_name in

    let sql = "DROP TABLE IF EXISTS ocmp_table" in
    let stmt = Mp_client.create_statement_from_string sql in
    let r = Mp_client.execute ~connection:connection ~statement:stmt () in
    let r = Mp_client.get_result r in
    let _ = Mp_client.get_result_ok r in
    (* let () = print_result sql r in *)

    let sql = "CREATE TABLE IF NOT EXISTS ocmp_table (id BIGINT AUTO_INCREMENT, col1 VARCHAR(255), col2 DECIMAL(30,10), PRIMARY KEY(id))" in
    let stmt = Mp_client.create_statement_from_string sql in
    let r = Mp_client.execute ~connection:connection ~statement:stmt () in
    let r = Mp_client.get_result r in
    let _ = Mp_client.get_result_ok r in
    (* let () = print_result sql r in *)

    let sql = "INSERT INTO ocmp_table (col1, col2) VALUES ('col1', 123.45)" in
    let stmt = Mp_client.create_statement_from_string sql in
    let r = Mp_client.execute ~connection:connection ~statement:stmt () in
    let r = Mp_client.get_result r in
    let _ = Mp_client.get_result_ok r in
    (* let () = print_result sql r in *)

    let params = [Mp_data.data_varstring "col2"; Mp_data.data_decimal (Num.num_of_string "98765/100")] in
    let sql = "INSERT INTO ocmp_table (col1, col2) VALUES (?, ?)" in
    let stmt = Mp_client.create_statement_from_string sql in
    let prep = Mp_client.prepare ~connection:connection ~statement:stmt in
    let r = Mp_client.execute ~connection:connection ~statement:prep ~params:params () in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep in
    let r = Mp_client.get_result r in
    let _ = Mp_client.get_result_ok r in
    (* let () = print_result sql r in *)

    let sql = "SELECT * FROM ocmp_table ORDER BY col1" in
    let stmt = Mp_client.create_statement_from_string sql in
    let r = Mp_client.execute ~connection:connection ~statement:stmt () in
    let r = Mp_client.get_result r in
    let _ = Mp_client.get_result_set r in
    (* let () = print_set sql r in  *)

    let params = [Mp_data.data_varstring "col1"] in
    let sql = "SELECT * FROM ocmp_table WHERE col1=?" in
    let stmt = Mp_client.create_statement_from_string sql in
    let prep = Mp_client.prepare ~connection:connection ~statement:stmt in
    let stmt = Mp_client.execute ~connection:connection ~statement:prep ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    let () = 
      try
        while true do
          let rows = Mp_client.fetch ~connection:connection ~statement:stmt () in
          let _ = Mp_client.get_fetch_result_set rows in
          () (* print_set sql rows *)
        done
      with
      | Mp_client.Fetch_no_more_rows -> () (* no more rows in the result *)
    in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep in

    let () = Mp_client.disconnect ~connection:connection in
    ()
  with
  | Mp_client.Error error -> (
      print_newline ();
      print_endline ("Exception: " ^ (Mp_client.error_exception_to_string error))
    )
