(* let host = "192.168.1.20"
let addr = Unix.inet_addr_of_string host
let port = 3306 *)
let sockaddr = Unix.ADDR_UNIX "/usr/jails/mariadb/var/run/mysql/mysql.sock"
let db_user = "user_ocaml_ocmp"
let db_password = "ocmp"
let db_name = "test_ocaml_ocmp_utf8"
