
type client_error = {
  client_error_errno : int;
  client_error_sqlstate : string;
  client_error_message : string;
}

exception Error of client_error
exception Fetch_no_more_rows

type configuration = {
  sockaddr : Unix.sockaddr;
  capabilities : Mp_capabilities.capabilities list;
  max_packet_size : Int64.t;
  charset_number : int;
  user : string;
  password : string;
  databasename : string;
}

type connection = {
  configuration : configuration;
  mutable channel : (in_channel * out_channel) option;
  mutable handshake : Mp_handshake.handshake option;
}

type dml_dcl_result = {
  affected_rows : Int64.t;
  insert_id : (Int64.t * Big_int.big_int);
  server_status : int;
  warning_count : int;
  message : string;
}

type prepare_result = {
  prepare_handler : Int64.t;
  prepare_nb_columns : int;
  prepare_nb_parameters : int;
  prepare_warning_count : int;
  prepare_parameters_fields : Mp_field_packet.field_packet list;
  prepare_parameters_names : Mp_field.field_name list;
  prepare_columns_fields : Mp_field_packet.field_packet list;
  prepare_columns_names : Mp_field.field_name list;
}

type result = 
    Result_set of Mp_result_set_packet.result_select
  | Result_ok of dml_dcl_result
  | Result_multiple of result list

type executable_statement = 
  | Created_statement of string
  | Prepared_statement of (string * prepare_result)

type execute_result = 
  | Executed_statement of (string * result)
  | Executed_prepared_statement of (string * prepare_result * result)
  | Executed_prepared_statement_with_cursor of (string * prepare_result)

let create_statement_from_string s =
  Created_statement s

let error_exception_to_string e = 
  Printf.sprintf "Errno: %u / Sql state: %s / Message: %s" e.client_error_errno e.client_error_sqlstate e.client_error_message

let dml_dcl_result_to_string r =
  let (insert_id_int64, insert_id_big_int) = r.insert_id in
  Printf.sprintf "Affected rows: %Lu / Insert id: (%Lu, %s) / Server status: %d / Warning count: %u : Message: %s"
    r.affected_rows insert_id_int64 (Big_int.string_of_big_int insert_id_big_int) r.server_status r.warning_count r.message

let configuration ~user ~password ~sockaddr ?(databasename = "") ?(max_packet_size) ?(charset) ?(capabilities) () =
  let max_packet_size = 
    match max_packet_size with
    | None -> Int64.of_int 1
    | Some x -> x
  in
  let encoding_number = 
    match charset with
    | None -> Mp_charset.charset_number (Mp_charset.Utf8, Mp_charset.Utf8_general_ci)
    | Some cn -> Mp_charset.charset_number cn
  in
  let default_capabilities = 
    [Mp_capabilities.Client_long_password;
     Mp_capabilities.Client_ignore_space;
     Mp_capabilities.Client_long_flag; 
     Mp_capabilities.Client_protocol_41;
     Mp_capabilities.Client_transactions;
     Mp_capabilities.Client_secure_connection;
     Mp_capabilities.Client_local_files;
     Mp_capabilities.Client_multi_results]
  in
  let default_capabilities = 
    if (String.length databasename > 0) then
      Mp_capabilities.Client_connect_with_db::default_capabilities
    else
      default_capabilities
  in
  let capabilities = 
    match capabilities with
    | None -> default_capabilities
    | Some x -> x
  in
  {
    sockaddr = sockaddr;
    capabilities = capabilities;
    max_packet_size = max_packet_size;
    charset_number = encoding_number;
    user = user;
    password = password;
    databasename = databasename;
  }

let send_packet ~packet ~ic ~oc ~sent ?(filter = None) ?(iter = None) ?(return_all_raw_mysql_data = false) ?(fields = []) () =
  let () = Bitstring.bitstring_to_chan packet oc in
  let () = flush oc in
  let result = Mp_result_packet.result_packet ic oc filter iter return_all_raw_mysql_data sent fields [] in
  match result with
  | [] -> failwith "No result (send_packet)"
  | (last_r, _) :: _ as r -> (
      match last_r with
      | Mp_result_packet.Result_packet_error e -> (
          raise (Error {
              client_error_errno = e.Mp_error_packet.error_errno;
              client_error_sqlstate = e.Mp_error_packet.error_sqlstate;
              client_error_message = e.Mp_error_packet.error_message
            })
        )
      | _ -> r
    )

let connect ~configuration ?(force = false) () = 
  let (channel, handshake) = 
    if force then (
      let (ic, oc) = Unix.open_connection configuration.sockaddr in
      let () =
        try
          Unix.setsockopt (Unix.descr_of_in_channel ic) Unix.TCP_NODELAY true ;
          Unix.setsockopt (Unix.descr_of_out_channel oc) Unix.TCP_NODELAY true ;
        with
        | Unix.Unix_error (Unix.EINVAL, _, _) -> () (* can fail if sockaddr is Unix.ADDR_UNIX *)
        | e -> raise e
      in
      let handshake = Mp_handshake.handshake_initialisation ic oc in
      let auth_plugin_name = "" in (* TODO: add in configuration *)
      let client_auth = 
        Mp_authentication.client_authentication_packet
          ~handshake:handshake ~capabilities:configuration.capabilities 
          ~max_packet_size:configuration.max_packet_size 
          ~charset_number:configuration.charset_number 
          ~user:configuration.user ~password:configuration.password 
          ~databasename:configuration.databasename
          ~auth_plugin_name:auth_plugin_name
      in
      let result = send_packet ~packet:client_auth ~ic:ic ~oc:oc ~sent:Mp_com.Authentication () in
      match result with
      | [] -> failwith "No result (connect)"
      | (result, _)::_ -> (
          match result with
          | Mp_result_packet.Result_packet_ok _ -> (Some (ic, oc), Some handshake)
          | Mp_result_packet.Result_packet_eof _ -> failwith "Old authentication scheme not supported"
          | _ -> failwith "Bad connect"
        )
    )
    else 
      (None, None)
  in
  {
    configuration = configuration;
    channel = channel;
    handshake = handshake;
  }

let reconnect ~connection = 
  match (connection.channel, connection.handshake) with
  | (Some _, Some _) -> ()
  | (_, _) -> (
      let c = connect ~configuration:connection.configuration ~force:true () in 
      let () = connection.channel <- c.channel in
      connection.handshake <- c.handshake
    )

let get_ic_oc ~connection = 
  let () = reconnect ~connection:connection in
  match connection.channel with
  | Some (i, o) -> (i,o)
  | None -> failwith "No communication channel"

let change_user ~connection ~user ~password ?(databasename = "") ?(charset) () =
  let (ic, oc) = get_ic_oc ~connection:connection in
  let configuration = connection.configuration in
  let handshake = 
    match connection.handshake with
      None -> failwith "Unable to change user (no handshake)"
    | Some h -> h
  in
  let encoding_number = 
    match charset with
    | None -> Mp_charset.charset_number (Mp_charset.Utf8, Mp_charset.Utf8_general_ci)
    | Some cn -> Mp_charset.charset_number cn
  in
  let auth_plugin_name = "" in (* TODO: add in configuration *)
  let bits = Mp_change_user.build_change_user ~handshake:handshake 
      ~user:user ~password:password 
      ~databasename:databasename ~charset_number:encoding_number 
      ~auth_plugin_name:auth_plugin_name in
  let change_user = Mp_com.com_change_user bits in
  try
    let result = send_packet ~packet:change_user ~ic:ic ~oc:oc ~sent:Mp_com.Change_user () in
    match result with
    | [] -> failwith "No result (change_user)"
    | (result, _)::_ -> (
        match result with
        | Mp_result_packet.Result_packet_ok _ -> (
            { configuration with 
                charset_number = encoding_number;
                user = user;
                password = password;
                databasename = databasename;
            }
          )
        | _ -> failwith "Unable to change user (bad answer)"
      )
  with
    | Mp_eof_packet.Bad_EOF_packet eof_packet -> (
        let auth_switch_request = Mp_auth_switch_request.auth_switch_request_packet_bits_without_0xFE_prefix eof_packet in
        let plugin_data = auth_switch_request.Mp_auth_switch_request.plugin_data in
        let encrypted_pwd = Mp_authentication.encode_client_password
          (Bitstring.string_of_bitstring plugin_data) password
        in
        (* /!\ : "1" in make_packet() because it's the second packet to send *)
        let response_packet = Mp_packet.make_packet 1 (Bitstring.bitstring_of_string encrypted_pwd) in
        let result = send_packet ~packet:response_packet ~ic ~oc
          ~sent:Mp_com.Client_response_auth_switch_request_plugin_mysql_native_password ()
        in
        match result with
        | [] -> failwith "No result (client_response_auth_switch_request_plugin_mysql_native_password)"
        | (result, _)::_ -> (
            match result with
            | Mp_result_packet.Result_packet_ok _ -> (
                { configuration with 
                    charset_number = encoding_number;
                    user = user;
                    password = password;
                    databasename = databasename;
                }
              )
            | _ -> failwith "Unable to change user (bad client_response_auth_switch_request_plugin_mysql_native_password)"
          )
    )

let reset_session ~connection =
  let configuration = connection.configuration in
  let _ = change_user ~connection
    ~user:configuration.user
    ~password:configuration.password
    ~databasename:configuration.databasename
    ~charset:(Mp_charset.number_charset configuration.charset_number) ()
  in ()

let reset_connection ~connection = 
  let (ic, oc) = get_ic_oc ~connection:connection in
  let bits = Mp_com.com_reset_connection in
  let result = send_packet ~packet:bits ~ic:ic ~oc:oc ~sent:Mp_com.Reset_connection () in
  match result with
  | [] -> failwith "No result (reset_connection)"
  | (result, _)::_ -> (
      match result with
      | Mp_result_packet.Result_packet_ok _ -> ()
      | _ -> failwith "Unable to reset connection (bad answer)"
    )

let use_database ~connection ~databasename = 
  let (ic, oc) = get_ic_oc ~connection:connection in
  let use_db = Mp_com.com_init_db databasename in
  let result = send_packet ~packet:use_db ~ic:ic ~oc:oc ~sent:Mp_com.Init_db () in 
  match result with
  | [] -> failwith "No result (use_database)"
  | (result, _)::_ -> (
      match result with
      | Mp_result_packet.Result_packet_ok _ -> ()
      | _ -> failwith "Bad use database"
    )

let query ~connection ~sql ?(filter = None) ?(iter = None) ?(return_all_raw_mysql_data = false) () = 
  let (ic, oc) = get_ic_oc ~connection:connection in
  let query = Mp_com.com_query sql in
  send_packet ~packet:query ~ic:ic ~oc:oc ~filter:filter ~iter:iter
    ~return_all_raw_mysql_data:return_all_raw_mysql_data ~sent:Mp_com.Query ()

let prepare ~connection ~statement = 
  match statement with 
  | Created_statement sql -> (
      let (ic, oc) = get_ic_oc ~connection:connection in
      let query = Mp_com.com_prepare sql in
      let result = send_packet ~packet:query ~ic:ic ~oc:oc ~sent:Mp_com.Prepare () in
      match result with
      | [] -> failwith "No result (prepare)"
      | (result_set, _)::_ -> (
          let result = 
            match result_set with
            | Mp_result_packet.Result_packet_prepare_ok p -> (
                { prepare_handler = p.Mp_ok_prepare_packet.ok_prepare_handler;
                  prepare_nb_columns = p.Mp_ok_prepare_packet.ok_prepare_nb_columns;
                  prepare_nb_parameters = p.Mp_ok_prepare_packet.ok_prepare_nb_parameters;
                  prepare_warning_count = p.Mp_ok_prepare_packet.ok_prepare_warning_count;
                  prepare_parameters_fields = p.Mp_ok_prepare_packet.ok_prepare_parameters_fields;
                  prepare_parameters_names = p.Mp_ok_prepare_packet.ok_prepare_parameters_names;
                  prepare_columns_fields = p.Mp_ok_prepare_packet.ok_prepare_columns_fields;
                  prepare_columns_names = p.Mp_ok_prepare_packet.ok_prepare_columns_names;
                }
              )
            | Mp_result_packet.Result_packet_error e -> (
                raise (Error {
                    client_error_errno = e.Mp_error_packet.error_errno;
                    client_error_sqlstate = e.Mp_error_packet.error_sqlstate;
                    client_error_message = e.Mp_error_packet.error_message
                  })
              )
            | _ -> assert false
          in
          Prepared_statement (sql, result)
        )
    )
  | Prepared_statement _ as p -> p (* no op if already prepared *)

let get_created_statement = function
  | Created_statement s -> s
  | _ -> failwith "No created statement"

let get_prepared_statement = function
  | Prepared_statement s -> s
  | _ -> failwith "No prepared statement"

let execute ~connection ~statement ?(filter = None) ?(iter = None) ?(return_all_raw_mysql_data = false) 
    ?(params = []) ?(bind = Mp_execute.Bind) ?(flag = Mp_execute.Cursor_type_no_cursor) () = 
  let result = 
    match statement with
    | Created_statement sql -> (
        query ~connection:connection ~sql:sql ~filter:filter ~iter:iter 
          ~return_all_raw_mysql_data:return_all_raw_mysql_data ()
      )
    | Prepared_statement (_, prepared) -> (
        let (ic, oc) = get_ic_oc ~connection:connection in
        let exec = Mp_execute.build_execute ~handler:prepared.prepare_handler 
            ~params:params ~fields:prepared.prepare_parameters_fields ~bind:bind ~flag:flag () in
        let query = Mp_com.com_execute exec in
        send_packet ~packet:query ~ic:ic ~oc:oc ~filter:filter ~iter:iter 
          ~return_all_raw_mysql_data:return_all_raw_mysql_data ~sent:Mp_com.Execute ()
      )
  in
  let result' = 
    match result with
    | [] -> failwith "No result (execute)"
    | (result_set, _)::[] -> (
        match result_set with
        | Mp_result_packet.Result_packet_result_set r ->
          Result_set r
        | Mp_result_packet.Result_packet_ok p ->
          Result_ok 
            { affected_rows = p.Mp_ok_packet.ok_affected_rows;
              insert_id = p.Mp_ok_packet.ok_insert_id;
              server_status = p.Mp_ok_packet.ok_server_status;
              warning_count = p.Mp_ok_packet.ok_warning_count;
              message = p.Mp_ok_packet.ok_message;
            }
        | Mp_result_packet.Result_packet_error e ->
          raise (Error {
              client_error_errno = e.Mp_error_packet.error_errno;
              client_error_sqlstate = e.Mp_error_packet.error_sqlstate;
              client_error_message = e.Mp_error_packet.error_message
            })
        | _ -> assert false
      )
    | l -> (
        let f acc (result_set, _) = 
          match result_set with
          | Mp_result_packet.Result_packet_result_set r -> 
            (Result_set r) :: acc
          | Mp_result_packet.Result_packet_ok p ->
            (Result_ok 
               { affected_rows = p.Mp_ok_packet.ok_affected_rows;
                 insert_id = p.Mp_ok_packet.ok_insert_id;
                 server_status = p.Mp_ok_packet.ok_server_status;
                 warning_count = p.Mp_ok_packet.ok_warning_count;
                 message = p.Mp_ok_packet.ok_message;
               }) :: acc
          | Mp_result_packet.Result_packet_error e ->
            raise (Error {
                client_error_errno = e.Mp_error_packet.error_errno;
                client_error_sqlstate = e.Mp_error_packet.error_sqlstate;
                client_error_message = e.Mp_error_packet.error_message
              })
          | _ -> assert false
        in
        Result_multiple (List.fold_left f [] l)
      )
  in
  match statement with
  | Created_statement sql -> 
    Executed_statement (sql, result')
  | Prepared_statement (sql, p) ->
    match flag with
    | Mp_execute.Cursor_type_no_cursor -> Executed_prepared_statement (sql, p, result')
    | _ -> Executed_prepared_statement_with_cursor (sql, p)

let get_result statement =
  let result = 
    match statement with
    | Executed_statement (_, result) -> result
    | Executed_prepared_statement (_, _, result) -> result
    | _ -> failwith "No result"
  in
  result

let get_result_multiple statement =
  let result = get_result statement in
  match result with
  | Result_multiple r -> r
  | _ -> failwith "No result multiple"

let get_result_set = function
  | Result_set r -> r
  | _ -> failwith "No result set"

let get_result_ok = function
  | Result_ok r -> r
  | _ -> failwith "No result ok"

let get_fetch_result_set = function
  | Result_set r -> r
  | _ -> failwith "No fetch result set"

let fetch ~connection ~statement ?(nb_rows = Int64.one) ?(filter = None) ?(iter = None) ?(return_all_raw_mysql_data = false) () = 
  let result = 
    match statement with
    | Executed_prepared_statement_with_cursor (_, prepared) -> (
        let (ic, oc) = get_ic_oc ~connection:connection in
        let fetch = Mp_fetch.build_fetch ~handler:prepared.prepare_handler ~nb_rows:nb_rows () in
        let query = Mp_com.com_fetch fetch in
        send_packet ~packet:query ~ic:ic ~oc:oc ~filter:filter ~iter:iter 
          ~return_all_raw_mysql_data:return_all_raw_mysql_data ~sent:Mp_com.Fetch ~fields:prepared.prepare_columns_fields ()
      )
    | _ -> failwith "Bad fetch attempt : non prepared statement or statement without cursor"
  in
  match result with
  | [] -> failwith "No result (fetch)"
  | (result_set, _)::_ -> (
      match result_set with
        Mp_result_packet.Result_packet_result_set r -> Result_set r
      | Mp_result_packet.Result_packet_eof _ -> raise Fetch_no_more_rows
      | Mp_result_packet.Result_packet_error e -> (
          raise (Error {
              client_error_errno = e.Mp_error_packet.error_errno;
              client_error_sqlstate = e.Mp_error_packet.error_sqlstate;
              client_error_message = e.Mp_error_packet.error_message
            })
        )
      | _ -> assert false
    )

let close_statement ~connection ~statement = 
  let prepared = 
    match statement with
    | Prepared_statement (_, prepared) -> prepared
    | Created_statement _ -> failwith "Unable to close non prepared statement"
  in
  let (_, oc) = get_ic_oc ~connection:connection in
  let handler = prepared.prepare_handler in
  let%bitstring bits = {|
      handler : Mp_bitstring.compute32 : int, unsigned, littleendian
    |}
  in
  let close_stmt = Mp_com.com_close_statement bits in
  (* no answer is expected (??) so we directly send the packet *)
  let () = Bitstring.bitstring_to_chan close_stmt oc in
  flush oc

let ping ~connection = 
  let (ic, oc) = get_ic_oc ~connection:connection in
  let ping = Mp_com.com_ping in
  let result = send_packet ~packet:ping ~ic:ic ~oc:oc ~sent:Mp_com.Ping () in
  match result with
  | [] -> failwith "No result (ping)"
  | (result, _)::_ -> ( 
      match result with
      | Mp_result_packet.Result_packet_ok _ -> ()
      | _ -> failwith "Unable to ping"
    )

let disconnect ~connection = 
  let (ic, oc) = get_ic_oc ~connection:connection in
  let quit = Mp_com.com_quit in
  (* no answer is expected so we directly send the packet *)
  let () = Bitstring.bitstring_to_chan quit oc in
  let () = flush oc in
  let () = Unix.close (Unix.descr_of_in_channel ic) in
  let () = connection.channel <- None in
  connection.handshake <- None
