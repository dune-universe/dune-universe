open Common

let _ =
  let tbl = load_config_file "./config" in
  let file = "sava.avi" in
  let cmd = find_cmd file tbl in
  assert (cmd = "mpv") ;
  let file = "savapas.fjeklsfhjkfhsek" in
  let cmd = find_cmd file tbl in
  assert (cmd = "xdg-open") ;
  Format.printf "Tests are OK !@."
