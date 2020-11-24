
let print_set sql r = 
  let print_row e = 
    let (id, col1, col2) = e in
    let s = Printf.sprintf "  id: %Lu\n  col1: %s\n  col2: %s\n  -- --" id col1 col2 in
    print_endline (s ^ "\n")
  in
  let () = print_endline ("Result set for the SQL statement \"" ^ sql ^ "\":\n") in
  List.iter print_row r 

let build_set r = 
  let col = Mysql.column r in
  let row x = ( Mysql.not_null Mysql.int642ml (col ~key:"id" ~row:x), 
                Mysql.not_null Mysql.str2ml (col ~key:"col1" ~row:x),
                Mysql.not_null Mysql.decimal2ml (col ~key:"col2" ~row:x)
              ) in
  let rec loop = function
    | None -> []
    | Some x -> row x :: loop (Mysql.fetch r)
  in
  loop (Mysql.fetch r)

let build_set_from_prepare r = 
  let row x = 
    ( Mysql.not_null Mysql.int642ml x.(0), 
      Mysql.not_null Mysql.str2ml x.(1),
      Mysql.not_null Mysql.decimal2ml x.(2)
    )
  in
  let rec loop = function
    | None -> []
    | Some x -> row x :: loop (Mysql.Prepared.fetch r)
  in
  loop (Mysql.Prepared.fetch r)

let run() = 

  let conf = {
    Mysql.dbhost = None;
    Mysql.dbname = None;
    Mysql.dbport = None;
    Mysql.dbpwd = Some Bench_config.db_password;
    Mysql.dbuser = Some Bench_config.db_user;
    Mysql.dbsocket = Some Bench_config.sockaddr;
  } in

  let db = Mysql.connect ~options:[] conf in

  let _ = Mysql.exec db "DROP TABLE IF EXISTS ocmp_table" in

  let _ = Mysql.exec db "CREATE TABLE IF NOT EXISTS ocmp_table (id BIGINT AUTO_INCREMENT, col1 VARCHAR(255), col2 DECIMAL(30,10), PRIMARY KEY(id))" in

  let _ = Mysql.exec db "INSERT INTO ocmp_table (col1, col2) VALUES ('col1', 123.45)" in

  let stmt = Mysql.Prepared.create db "INSERT INTO ocmp_table (col1, col2) VALUES (?, ?)" in
  let params = Array.make 2 "" in
  let () = params.(0) <- "col2" in
  let () = params.(1) <- Mysql.ml2decimal "98765/100" in
  let _ = Mysql.Prepared.execute stmt params in
  let () = Mysql.Prepared.close stmt in

  let sql = "SELECT * FROM ocmp_table ORDER BY col1" in
  let r = Mysql.exec db sql in
  let _ = build_set r in
  (* let () = print_set sql r in *)

  let sql = "SELECT * FROM ocmp_table WHERE col1=?" in
  let stmt = Mysql.Prepared.create db sql in
  let params = Array.make 1 "" in
  let () = params.(0) <- "col1" in
  let r = Mysql.Prepared.execute stmt params in
  let _ = build_set_from_prepare r in
  (* let () = print_set sql r in *)
  let () = Mysql.Prepared.close stmt in

  let () = Mysql.disconnect db in

  ()
