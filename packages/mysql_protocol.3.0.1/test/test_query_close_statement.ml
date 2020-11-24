open OUnit
open Mysql_protocol

let test1 connection = 
  let () = 
    let sql = "SELECT * FROM test_ocmp WHERE f_autoinc_not_null_no_def > 0" in
    let stmt = Mp_client.create_statement_from_string sql in
    let prep10 = Mp_client.prepare ~connection:connection ~statement:stmt in
    let _ = Mp_client.execute ~connection:connection ~statement:prep10 () in
    let () = 
      assert_equal ~msg:sql 
        (())
        (Mp_client.close_statement ~connection:connection ~statement:prep10)
    in
    let () = 
      let result = 
        try
          let _ = Mp_client.execute ~connection:connection ~statement:prep10 () in
          false;
        with
        | Mp_client.Error e -> (
            let errno_ok = 1243 in
            let state_ok = "HY000" in
            let msg_ok = "Unknown prepared statement handler" in
            let errno_e = e.Mp_client.client_error_errno in
            let state_e = e.Mp_client.client_error_sqlstate in
            let msg_e = e.Mp_client.client_error_message in
            (errno_ok = errno_e) && (state_ok = state_e) && ((String.sub msg_e 0 34) = msg_ok)
          )
        | _ -> false
      in
      assert_equal ~msg:sql true result
    in
    ()
  in
  ()

let test _ connection _ = 
  try
    test1 connection
  with
  | Mp_client.Error err as e -> (
      let () = prerr_endline (Mp_client.error_exception_to_string err) in
      raise e
    )
