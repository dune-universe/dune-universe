open OUnit
open Mysql_protocol

let test1 connection = 
  let () = 
    assert_equal ~msg:"Ping"
      (())
      (Mp_client.ping ~connection:connection)
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
