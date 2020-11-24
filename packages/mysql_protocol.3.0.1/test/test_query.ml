open Mysql_protocol

let try_query ~f ~sql = 
  try
    Test_benchmark.time (fun _ -> f) sql
  with
  | Mp_client.Error err as e -> (
      let () = print_endline (Printf.sprintf "%s : Error : %s" sql (Mp_client.error_exception_to_string err)) in
      raise e
    )
