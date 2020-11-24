type capabilities =
    Client_long_password
  | Client_found_rows
  | Client_long_flag
  | Client_connect_with_db
  | Client_no_schema
  | Client_compress
  | Client_odbc
  | Client_local_files
  | Client_ignore_space
  | Client_protocol_41
  | Client_interactive
  | Client_ssl
  | Client_ignore_sigpipe
  | Client_transactions
  | Client_reserved
  | Client_secure_connection
  | Client_multi_statements
  | Client_multi_results

let capabilities_to_string capabilities = 
  let build_string acc capability =
    match capability with
      Client_long_password -> acc ^ "CLIENT_LONG_PASSWORD,"
    | Client_found_rows -> acc ^ "CLIENT_FOUND_ROWS,"
    | Client_long_flag -> acc ^ "CLIENT_LONG_FLAG,"
    | Client_connect_with_db -> acc ^ "CLIENT_CONNECT_WITH_DB,"
    | Client_no_schema -> acc ^ "CLIENT_NO_SCHEMA,"
    | Client_compress -> acc ^ "CLIENT_COMPRESS,"
    | Client_odbc -> acc ^ "CLIENT_ODBC,"
    | Client_local_files -> acc ^ "CLIENT_LOCAL_FILES,"
    | Client_ignore_space -> acc ^ "CLIENT_IGNORE_SPACE,"
    | Client_protocol_41 -> acc ^ "CLIENT_PROTOCOL_41,"
    | Client_interactive -> acc ^ "CLIENT_INTERACTIVE,"
    | Client_ssl -> acc ^ "CLIENT_SSL,"
    | Client_ignore_sigpipe -> acc ^ "CLIENT_IGNORE_SIGPIPE,"
    | Client_transactions -> acc ^ "CLIENT_TRANSACTIONS,"
    | Client_reserved -> acc ^ "CLIENT_RESERVED,"
    | Client_secure_connection -> acc ^ "CLIENT_SECURE_CONNECTION,"
    | Client_multi_statements -> acc ^ "CLIENT_MULTI_STATEMENTS,"
    | Client_multi_results -> acc ^ "CLIENT_MULTI_RESULTS"
  in
  let s = List.fold_left build_string "" capabilities in
  if String.length s > 0 then String.sub s 0 ((String.length s) - 1) else s

(* /!\ : NEED CHECK !!!!!! *)
let decode_server_capabilities bits = 
  match%bitstring bits with
(* 
   | {
   long_password : 1;
   found_rows : 1;
   long_flag : 1;
   connect_with_db : 1;
   no_schema : 1;
   compress : 1;
   odbc : 1;
   local_files : 1;
   ignore_space : 1;
   protocol_41 : 1;
   interactive : 1;
   ssl : 1;
   ignore_sigpipe : 1;
   transactions : 1;
   reserved : 1;
   secure_connection : 1 
   }
*)
| {|
  secure_connection : 1;
  reserved : 1;
  transactions : 1;
  ignore_sigpipe : 1;
  ssl : 1;
  interactive : 1;
  protocol_41 : 1;
  ignore_space : 1;
  local_files : 1;
  odbc : 1;
  compress : 1;
  no_schema : 1;
  connect_with_db : 1;
  long_flag : 1;
  found_rows : 1;
  long_password : 1 |} ->
  let l = [] in
  let l = if long_password then Client_long_password::l else l in
  let l = if found_rows then Client_found_rows::l else l in
  let l = if long_flag then Client_long_flag::l else l in
  let l = if connect_with_db then Client_connect_with_db::l else l in
  let l = if no_schema then Client_no_schema::l else l in
  let l = if compress then Client_compress::l else l in
  let l = if odbc then Client_odbc::l else l in
  let l = if local_files then Client_local_files::l else l in
  let l = if ignore_space then Client_ignore_space::l else l in
  let l = if protocol_41 then Client_protocol_41::l else l in
  let l = if interactive then Client_interactive::l else l in
  let l = if ssl then Client_ssl::l else l in
  let l = if ignore_sigpipe then Client_ignore_sigpipe::l else l in
  let l = if transactions then Client_transactions::l else l in
  let l = if reserved then Client_reserved::l else l in
  let l = if secure_connection then Client_secure_connection::l else l in
  l

(* 
   /!\ : WARNING : the wiki doc includes several other constants
                   greater than a 16 bits value !!

                   Anyway, it seems ok : 

                     http://bugs.mysql.com/bug.php?id=42268

   CLIENT_MULTI_STATEMENTS 65536   /* Enable/disable multi-stmt support */
   CLIENT_MULTI_RESULTS    131072  /* Enable/disable multi-results */
   ...

*)
let encode_client_capabilities capabilities_list = 
  let sum_capability sum c =
    match c with 
    | Client_long_password -> sum + 1
    | Client_found_rows -> sum + 2
    | Client_long_flag -> sum + 4
    | Client_connect_with_db -> sum + 8
    | Client_no_schema -> sum + 16
    | Client_compress -> sum + 32
    | Client_odbc -> sum + 64
    | Client_local_files -> sum + 128
    | Client_ignore_space -> sum + 256
    | Client_protocol_41 -> sum + 512
    | Client_interactive -> sum + 1024
    | Client_ssl -> sum + 2048
    | Client_ignore_sigpipe -> sum + 4096
    | Client_transactions -> sum + 8192
    | Client_reserved -> sum + 16384
    | Client_secure_connection -> sum + 32768
    | Client_multi_statements -> sum + 65536
    | Client_multi_results -> sum + 131072
  in
  List.fold_left sum_capability 0 capabilities_list
