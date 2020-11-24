open Mysql_protocol

let test1 host db_name encoding = 
  let () = 
    let (_, version, connection_type, db_user, db_password) = host in
    (* configuration *)
    let sockaddr = match connection_type with
      | Test_types.CInet (_, addr, port) -> Unix.ADDR_INET(addr, port)
      | Test_types.CUnix path -> Unix.ADDR_UNIX path
    in
    let config = Mp_client.configuration 
        ~user:db_user ~password:db_password ~sockaddr:sockaddr 
        ~databasename:db_name ~charset:encoding () in
    (* connection *)
    let connection = Mp_client.connect ~configuration:config ~force:true () in
    (* use database *)
    let () = Mp_client.use_database ~connection:connection ~databasename:db_name in
    (* delete (non prepared) to clean database *)
    let stmt = Mp_client.create_statement_from_string "DROP TABLE IF EXISTS test_ocmp_client" in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    let stmt = Mp_client.create_statement_from_string "DROP TABLE IF EXISTS test_ocmp_client_tmp" in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* create table (non prepared) *)
    let stmt = Mp_client.create_statement_from_string "CREATE TABLE IF NOT EXISTS test_ocmp_client (id BIGINT AUTO_INCREMENT NOT NULL, PRIMARY KEY (id))" in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* create table (prepared) *)
    let stmt = Mp_client.create_statement_from_string "CREATE TABLE IF NOT EXISTS test_ocmp_client_tmp (xx INT)" in
    let prep10 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep10 () in
    (* drop table (non prepared) *)
    let stmt = Mp_client.create_statement_from_string "DROP TABLE IF EXISTS test_ocmp_client_tmp" in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* alter table (non prepared) *)
    let stmt = Mp_client.create_statement_from_string "ALTER TABLE test_ocmp_client ADD name VARCHAR(250) DEFAULT NULL" in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* alter table (prepared) *)
    let stmt = Mp_client.create_statement_from_string "ALTER TABLE test_ocmp_client ADD number DECIMAL(20,5) DEFAULT 0" in
    let prep20 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep20 () in
    (* grant (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("GRANT SELECT ON " ^ db_name ^ ".test_ocmp_client TO '" ^ db_user ^ "'@'localhost'") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* grant (prepared) *)
    let () = 
      (* = 5.0.95 : Errno: 1295 / Sql state: HY000 / Message: This command is not supported in the prepared statement protocol yet *)
      if (version > 5095) then (
        let stmt = Mp_client.create_statement_from_string ("GRANT UPDATE ON " ^ db_name ^ ".test_ocmp_client TO '" ^ db_user ^ "'@'localhost'") in
        let prep30 = Mp_client.prepare ~connection:connection ~statement:stmt in
        let _ = Mp_client.execute ~connection:connection ~statement:prep30 () in
        let () = Mp_client.close_statement ~connection:connection ~statement:prep30 in
        ()
      )
    in
    (* show (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("SHOW COLUMNS FROM test_ocmp_client") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* show (prepared) *)
    let stmt = Mp_client.create_statement_from_string ("SHOW COLUMNS FROM test_ocmp_client") in
    let prep40 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep40 () in
    (* insert (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("INSERT INTO test_ocmp_client (name, number) VALUES ('nameX', 148.52)") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* insert (prepared + params) *)
    let params = [Mp_data.data_varstring "name'2"; Mp_data.data_decimal (Num.num_of_string "26895/100")] in
    let stmt = Mp_client.create_statement_from_string ("INSERT INTO test_ocmp_client (name, number) VALUES (?, ?)") in
    let prep50 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep50 ~params:params () in
    (* update (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("UPDATE test_ocmp_client SET name='name\\'1' WHERE name='nameX'") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* update (prepared + params) *)
    let params = [Mp_data.data_decimal (Num.num_of_string "1")] in
    let stmt = Mp_client.create_statement_from_string ("UPDATE test_ocmp_client SET number=number+?") in
    let prep60 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep60 ~params:params () in
    (* select (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("SELECT * FROM test_ocmp_client ORDER BY id") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* select (prepared + params) *)
    let params = [Mp_data.data_longlongint Big_int.unit_big_int] in
    let stmt = Mp_client.create_statement_from_string ("SELECT * FROM test_ocmp_client WHERE id=?") in
    let prep70 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let stmt = Mp_client.execute ~connection:connection ~statement:prep70 ~params:params ~flag:Mp_execute.Cursor_type_read_only () in
    (* fetch *)
    let _ = Mp_client.fetch ~connection:connection ~statement:stmt () in
    (* ping *)
    let () = Mp_client.ping ~connection:connection in
    (* change user *)
    let _ = Mp_client.change_user ~connection:connection ~user:"u_ocmp_npauth_2" ~password:"ocmpnpauth2"
          ~databasename:connection.configuration.databasename () in
    (* reset session *)
    let () = Mp_client.reset_session ~connection:connection in
    (* reset connection *)
    let () = Mp_client.reset_connection ~connection:connection in
    (* select (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("SELECT * FROM test_ocmp_client ORDER BY id LIMIT 1") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* select (prepared) *)
    let stmt = Mp_client.create_statement_from_string ("SELECT * FROM test_ocmp_client WHERE id=2") in
    let prep80 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep80 () in
    (* reuse the prepared statement *)
    let _ = Mp_client.execute ~connection:connection ~statement:prep80 () in
    (* close prepared statements *)
    let () = Mp_client.close_statement ~connection:connection ~statement:prep10 in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep20 in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep40 in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep50 in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep60 in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep70 in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep80 in
    (* try using closed prepared statements *)
    let () = 
      try
        let _ = Mp_client.execute ~connection:connection ~statement:prep80 () in
        ()
      with
      | Mp_client.Error error -> 
        if (error.Mp_client.client_error_errno <> 1243) then
          assert false
    in
    (* select (non prepared) + conversion to OCaml value *)
    let stmt = Mp_client.create_statement_from_string ("SELECT name FROM test_ocmp_client WHERE id=1") in
    let r = Mp_client.execute ~connection:connection ~statement:stmt () in
    let r = Mp_client.(get_result_set(get_result r)) in
    let (_, rows) = r.Mp_result_set_packet.rows in
    let row = List.nth rows 0 in
    let data = List.nth row 0 in
    let s =
        match Mp_data.to_ocaml_string data with
        | None -> assert false
        | Some v -> v
    in
    let () = if (s <> "name'1") then assert false in
    (* delete (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("DELETE FROM test_ocmp_client WHERE id=1") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* delete (prepared) *)
    let params = [Mp_data.data_longlongint (Big_int.big_int_of_int 2)] in
    let stmt = Mp_client.create_statement_from_string ("DELETE FROM test_ocmp_client WHERE id=?") in
    let prep90 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep90 ~params:params () in
    (* select (non prepared) *)
    let stmt = Mp_client.create_statement_from_string ("SELECT * FROM test_ocmp_client") in
    let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
    (* select (prepared) *)
    let stmt = Mp_client.create_statement_from_string ("SELECT * FROM test_ocmp_client") in
    let prep100 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep100 () in
    (* catch MySQL error *)
    let stmt = Mp_client.create_statement_from_string ("BAD SQL QUERY") in
    let () = 
      try
        let _ = Mp_client.execute ~connection:connection ~statement:stmt () in
        ()
      with
      | Mp_client.Error error -> 
        if (error.Mp_client.client_error_errno <> 1064) then
          assert false
    in
    (* close prepared statements *)
    let () = Mp_client.close_statement ~connection:connection ~statement:prep90 in
    let () = Mp_client.close_statement ~connection:connection ~statement:prep100 in
    (* disconnect *)
    let () = Mp_client.disconnect ~connection:connection in
    ()
  in
  ()

let test host encoding _ = 
  let module F = (
    val (
      match encoding with
      | (Mp_charset.Latin1, _) -> (
          let module E = struct
            include Fixture_latin1
          end
          in (module E : Fixture.FIXTURE)
        )
      | (Mp_charset.Utf8, _) -> (
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
    let () = test1 host F.db_name encoding in
    ()
  with
  | Mp_client.Error err as e -> (
      let () = prerr_endline (Mp_client.error_exception_to_string err) in
      raise e
    )
