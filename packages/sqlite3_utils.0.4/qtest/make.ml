
let () =
  if Sys.command "which qtest > /dev/null" <> 0 then (
    (* create empty file *)
  ) else (
    let files = "../src/sqlite3_utils.ml ../src/sqlite3_utils.mli" in
    let cmd =
      Printf.sprintf "qtest extract %s 2>/dev/null"
        files
    in
    exit (Sys.command cmd)
  )
