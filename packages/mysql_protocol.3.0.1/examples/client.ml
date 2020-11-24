
(**
   A client session example.
*)

module Mp_client = Mysql_protocol.Mp_client
module Mp_data = Mysql_protocol.Mp_data
module Mp_execute = Mysql_protocol.Mp_execute
module Mp_result_set_packet = Mysql_protocol.Mp_result_set_packet
module Mp_capabilities = Mysql_protocol.Mp_capabilities

let run() = 
  (* helper function to display ok result (INSERT, UPDATE... result) *)
  let print_result sql r = 
    print_endline ("Result of the SQL statement \"" ^ sql ^ "\": \n  " ^ (Mp_client.dml_dcl_result_to_string r) ^ "\n")
  in

  (* helper functions to display set result (SELECT result) *)
  let print_row fields row = 
    let print_data f = 
      let (field_name, field_pos) = f in
      let data = List.nth row field_pos in
      print_endline ("  " ^ field_name ^ ": " ^ (Option.value (Mp_data.to_string data) ~default:""))
    in
    let () = List.iter print_data fields in
    print_endline "  -- --  "
  in
  let print_set sql r = 
    let (fields, rows) = r.Mp_result_set_packet.rows in
    let () = print_endline ("Result set for the SQL statement \"" ^ sql ^ "\": \n") in
    let print_rows =   
      let () = List.iter (print_row fields) rows in
      print_newline ()
    in
    print_rows
  in

  (* server address *)
  (* let addr = Unix.inet_addr_of_string "192.168.1.20" in *)

  (* server port *)
  (* let port = 3306 in *)

  (* let sockaddr = Unix.ADDR_INET(addr, port) in *)
  let sockaddr = Unix.ADDR_UNIX "/usr/jails/mariadb/var/run/mysql/mysql.sock" in

  (* MySQL user login *)
  let db_user = "user_ocaml_ocmp" in
  let db_user_2 = "u_ocmp_npauth" in

  (* MySQL user password *)
  let db_password = "ocmp" in
  let db_password_2 = "ocmpnpauth" in

  (* database name *)
  let db_name = "test_ocaml_ocmp_utf8" in

  (* configuration *)
  let config = Mp_client.configuration ~user:db_user ~password:db_password ~sockaddr:sockaddr ~databasename:db_name () in

  (* connection *)
  let connection = Mp_client.connect ~configuration:config () in

  (* use database *)
  let () = Mp_client.use_database ~connection:connection ~databasename:db_name in

  (* delete table with a non prepared statement to have a clean database *)
  let sql = "DROP TABLE IF EXISTS ocmp_table" in
  let stmt = Mp_client.create_statement_from_string sql in
  let r = Mp_client.execute ~connection:connection ~statement:stmt () in
  let r = Mp_client.get_result r in
  let r = Mp_client.get_result_ok r in
  let () = print_result sql r in

  (* create table with a non prepared statement *)
  let sql = "CREATE TABLE IF NOT EXISTS ocmp_table (id BIGINT AUTO_INCREMENT, col1 VARCHAR(255), col2 DECIMAL(30,10), PRIMARY KEY(id))" in
  let stmt = Mp_client.create_statement_from_string sql in
  let r = Mp_client.execute ~connection:connection ~statement:stmt () in
  let r = Mp_client.get_result r in
  let r = Mp_client.get_result_ok r in
  let () = print_result sql r in

  (* send non prepared SQL statement *)
  let sql = "INSERT INTO ocmp_table (col1, col2) VALUES ('col1', 123.45)" in
  let stmt = Mp_client.create_statement_from_string sql in
  let r = Mp_client.execute ~connection:connection ~statement:stmt () in
  let r = Mp_client.get_result r in
  let r = Mp_client.get_result_ok r in
  let () = print_result sql r in

  (* send prepared SQL statement with params *)
  let params = [Mp_data.data_varstring "col2"; Mp_data.data_decimal (Num.num_of_string "98765/100")] in
  let sql = "INSERT INTO ocmp_table (col1, col2) VALUES (?, ?)" in
  let stmt = Mp_client.create_statement_from_string sql in
  let prep = Mp_client.prepare ~connection:connection ~statement:stmt in
  let r = Mp_client.execute ~connection:connection ~statement:prep ~params:params () in
  let () = Mp_client.close_statement ~connection:connection ~statement:prep in
  let r = Mp_client.get_result r in
  let r = Mp_client.get_result_ok r in
  let () = print_result sql r in

  (* send non prepared SELECT statement *)
  let sql = "SELECT * FROM ocmp_table ORDER BY col1" in
  let stmt = Mp_client.create_statement_from_string sql in
  let r = Mp_client.execute ~connection:connection ~statement:stmt () in
  let r = Mp_client.get_result r in
  let r = Mp_client.get_result_set r in
  let () = print_set sql r in 

  (* send prepared SELECT statement with params but no fetch *)
  let params = [Mp_data.data_decimal (Num.num_of_string "98765/100")] in
  let sql = "SELECT * FROM ocmp_table WHERE col2=?" in
  let stmt = Mp_client.create_statement_from_string sql in
  let prep = Mp_client.prepare ~connection:connection ~statement:stmt in
  let r = Mp_client.execute ~connection:connection ~statement:prep ~params:params () in
  let () = Mp_client.close_statement ~connection:connection ~statement:prep in
  let r = Mp_client.get_result r in
  let r = Mp_client.get_result_set r in
  let () = print_set sql r in 

  (* send prepared SELECT statement with params and fetch the result *)
  let params = [Mp_data.data_varstring "col1"] in
  let sql = "SELECT * FROM ocmp_table WHERE col1=?" in
  let stmt = Mp_client.create_statement_from_string sql in
  let prep = Mp_client.prepare ~connection:connection ~statement:stmt in
  let stmt = Mp_client.execute ~connection:connection ~statement:prep ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
  let () = 
    try
      while true do
        let rows = Mp_client.fetch ~connection:connection ~statement:stmt () in
        let rows = Mp_client.get_fetch_result_set rows in
        print_set sql rows
      done
    with
    | Mp_client.Fetch_no_more_rows -> () (* no more rows in the result *)
  in
  let () = Mp_client.close_statement ~connection:connection ~statement:prep in

  (* send non prepared SELECT statement and embed the print function *)
  let sql = "SELECT * FROM ocmp_table ORDER BY col1" in
  let stmt = Mp_client.create_statement_from_string sql in
  let () = print_endline ("Result set for the SQL statement \"" ^ sql ^ "\" (print function embedded): \n") in
  let _ = Mp_client.execute ~connection:connection ~statement:stmt ~iter:(Some print_row) () in

  (* PING server *)
  let () = Mp_client.ping ~connection:connection in

  (* change user *)
  let _ = Mp_client.change_user ~connection:connection ~user:db_user_2 ~password:db_password_2 ~databasename:db_name () in

  (* reset session (equivalent to a disconnect and reconnect) *)
  let () = Mp_client.reset_session ~connection:connection in

  (* reset connection without re-authentication *)
  let () = Mp_client.reset_connection ~connection:connection in

  (* catch MySQL error *)
  let stmt = Mp_client.create_statement_from_string ("BAD SQL QUERY") in
  let () =
    try
      let _ = Mp_client.execute ~connection:connection ~statement:stmt () in 
      ()
    with
    | Mp_client.Error error ->
      print_newline ();
      print_endline ("This is a test to show how to catch a MySQL error, the exception is: " ^ (Mp_client.error_exception_to_string error));
      print_newline ();
  in

  (* create and call a procedure *)
  let sql = "DROP PROCEDURE IF EXISTS ocmp_proc" in
  let stmt = Mp_client.create_statement_from_string sql in
  let _ = Mp_client.execute ~connection:connection ~statement:stmt () in 
  let sql = "CREATE PROCEDURE ocmp_proc() BEGIN SELECT * FROM ocmp_table; END" in
  let stmt = Mp_client.create_statement_from_string sql in
  let r = Mp_client.execute ~connection:connection ~statement:stmt () in
  let r = Mp_client.get_result r in
  let r = Mp_client.get_result_ok r in
  let () = print_result sql r in
  let sql = "CALL ocmp_proc()" in
  let stmt = Mp_client.create_statement_from_string sql in
  let r = Mp_client.execute ~connection:connection ~statement:stmt () in
  let r = Mp_client.get_result_multiple r in
  let f e =
    try
      let rs = Mp_client.get_result_set e in
      print_set sql rs
    with
    | Failure _ ->
        let rs = Mp_client.get_result_ok e in
        let affected_rows = rs.Mp_client.affected_rows in
        print_endline (Printf.sprintf "Result OK: affected rows=%Ld" affected_rows)
  in
  let () = List.iter f r in

  (* disconnect *)
  let () = Mp_client.disconnect ~connection:connection in
  ()
