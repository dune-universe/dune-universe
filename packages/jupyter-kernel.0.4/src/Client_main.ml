
(* This file is free software. See file "license" for more details. *)

(** {1 Main for Client} *)

open Lwt.Infix

module C = Client
module Proto_j = Protocol_j

(*******************************************************************************)
(* command line *)

let connection_file_name = ref ""
let completion = ref false
let object_info = ref false
let init_file = ref ""

let ci_stdin = ref 50000
let ci_shell = ref 50001
let ci_iopub = ref 50002
let ci_heartbeat = ref 50003
let ci_control = ref 50004
let ci_transport = ref "tcp"
let ci_ip_addr = ref ""

let mk_args ~additional_args () =
  Arg.(align
      (additional_args @
         [
           "--log", String (Log.open_log_file), " <file> open log file";
           "--connection-file", Set_string(connection_file_name),
           " <filename> connection file name";
           "--init", Set_string(init_file), " <file> load <file> instead of default init file";
           "--completion", Set(completion), " enable tab completion";
           "--object-info", Set(object_info), " enable introspection";
           (* pass connection info through command line *)
           "--ci-stdin", Set_int(ci_stdin), " (connection info) stdin zmq port";
           "--ci-iopub", Set_int(ci_iopub), " (connection info) iopub zmq port";
           "--ci-shell", Set_int(ci_shell), " (connection info) shell zmq port";
           "--ci-control", Set_int(ci_control), " (connection info) control zmq port";
           "--ci-heartbeat", Set_int(ci_heartbeat), " (connection info) heartbeat zmq port";
           "--ci-transport", Set_string(ci_transport), " (connection info) transport";
           "--ci-ip", Set_string(ci_ip_addr), " (connection info) ip address"
         ]))

let mk_connection_info () : Proto_j.connection_info =
  if !ci_ip_addr <> "" then
    (* get configuration parameters from command line *)
    { Proto_j.
      stdin_port = !ci_stdin;
      ip = !ci_ip_addr;
      control_port = !ci_control;
      hb_port = !ci_heartbeat;
      signature_scheme = "hmac-sha256";
      key = "";
      shell_port = !ci_shell;
      transport = !ci_transport;
      iopub_port = !ci_iopub;
    }
  else (
    (* read from configuration files *)
    let f_conn_info =
      try open_in !connection_file_name
      with _ ->
        failwith ("Failed to open connection file: '" ^
            !connection_file_name ^ "'")
    in
    let state = Yojson.init_lexer () in
    let lex = Lexing.from_channel f_conn_info in
    let conn = Proto_j.read_connection_info state lex in
    let () = close_in f_conn_info in
    conn
  )

let main_loop connection_info kernel =
  try%lwt
    let sockets = Sockets.open_sockets connection_info in
    let key = connection_info.Proto_j.key in
    let key = if key="" then None else Some key in
    let sh = C.make ?key sockets kernel in
    C.run sh;
    >>= function
    | C.Run_stop ->
      Log.log "Done.\n";
      Sockets.close_sockets sockets
    | C.Run_restart ->
      Log.log "Done (restart).\n";
      Sockets.close_sockets sockets
  with e ->
    Log.log (Printf.sprintf "Exception: %s\n" (Printexc.to_string e));
    Log.log "Dying.\n";
    Lwt.fail Exit

type config = { connection_info: Proto_j.connection_info }

let mk_config ?(additional_args=[]) ~usage () : config =
  let args = mk_args ~additional_args () in
  Arg.parse
    args
    (fun s -> failwith ("invalid anonymous argument: " ^ s))
    usage;
  { connection_info = mk_connection_info () }

let main ~(config : config) ~kernel =
  let%lwt() = Lwt_io.printf "Starting kernel for `%s`\n" kernel.C.Kernel.language in
  Log.log "start main...\n";
  main_loop config.connection_info kernel >|= fun () ->
  Log.log "client_main: exiting\n"
