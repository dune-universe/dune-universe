type vendor =
  | MySQL
  | MariaDB

type connection_type = 
  | CInet of (string * Unix.inet_addr * int)
  | CUnix of string

let vendor_to_string v =
	match v with
	| MySQL -> "MySQL"
	| MariaDB -> "MariaDB"
