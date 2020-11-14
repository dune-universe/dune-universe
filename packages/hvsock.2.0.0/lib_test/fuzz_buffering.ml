(* Choose random payload sizes and test the buffering and thread handling *)

let () =
  let rec forever () =
    let to_write_client = Random.int (1024 * 1024 * 10) in
    let to_write_server = Random.int (1024 * 1024 * 10) in
    Buffering.test_read_write to_write_client to_write_server ;
    Printf.fprintf stderr ".%!" ;
    forever ()
  in
  forever ()
