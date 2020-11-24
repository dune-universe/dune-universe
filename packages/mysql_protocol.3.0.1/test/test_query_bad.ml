open OUnit
open Mysql_protocol

let test vendor connection _ =
  let () = 
    let sql = "SELC * FROM test_ocmp LIMIT 1" in
    let stmt = Mp_client.create_statement_from_string sql in
    assert_raises ~msg:sql 
      (Mp_client.Error {
          Mp_client.client_error_errno = 1064; 
          Mp_client.client_error_sqlstate = "42000"; 
          Mp_client.client_error_message = "You have an error in your SQL syntax; check the manual that corresponds to your " ^ (Test_types.vendor_to_string vendor) ^ " server version for the right syntax to use near '" ^ sql ^ "' at line 1"
        } ) 
      (fun _ -> (Test_query.try_query ~f:(Mp_client.execute ~connection:connection ~statement:stmt ()) ~sql:sql))
  in
  let () = 
    let sql = "SELECT * FROM test_ocmp WHERE f_varstring_null_no_def='" in
    let stmt = Mp_client.create_statement_from_string sql in
    assert_raises ~msg:sql 
      (Mp_client.Error {
          Mp_client.client_error_errno = 1064; 
          Mp_client.client_error_sqlstate = "42000"; 
          Mp_client.client_error_message = "You have an error in your SQL syntax; check the manual that corresponds to your " ^ (Test_types.vendor_to_string vendor) ^ " server version for the right syntax to use near ''' at line 1"
        } ) 
      (fun _ -> (Test_query.try_query ~f:(Mp_client.execute ~connection:connection ~statement:stmt ()) ~sql:sql))
  in
  let () = 
    let sql = "ALTER TABLE test_ocmp ADD f_varstring_null_no_def INT" in
    let stmt = Mp_client.create_statement_from_string sql in
    assert_raises ~msg:sql 
      (Mp_client.Error {
          Mp_client.client_error_errno = 1060; 
          Mp_client.client_error_sqlstate = "42S21"; 
          Mp_client.client_error_message = "Duplicate column name 'f_varstring_null_no_def'"
        } ) 
      (fun _ -> (Test_query.try_query ~f:(Mp_client.execute ~connection:connection ~statement:stmt ()) ~sql:sql))
  in
  ()
