open Common

let _ =
  let config_file = check_config_file () in
  let tbl = load_config_file config_file in
  let file = check_file () in
  let cmd = find_cmd file tbl in
  let cmd = Format.sprintf "setsid %s %s && exit" cmd (Filename.quote file) in
  (* Format.printf "%s@." cmd; *)
  ignore (Sys.command cmd)
