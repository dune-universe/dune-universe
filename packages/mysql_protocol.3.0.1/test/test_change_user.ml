open OUnit
open Mysql_protocol

let test1 connection = 
  let () = 
    assert_equal ~msg:"Reset session"
      (())
      (let _ =
        Mp_client.change_user ~connection:connection ~user:"u_ocmp_npauth_2" ~password:"ocmpnpauth2"
          ~databasename:connection.configuration.databasename () in
          ())
  in
  ()

let test connection _ = 
  try
    test1 connection
  with
  | Mp_client.Error err as e -> (
      let () = prerr_endline (Mp_client.error_exception_to_string err) in
      raise e
    )
