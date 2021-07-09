(*-----------------------------------------------------------------------
 ** Copyright (C) - Verimag.
 ** This file may only be copied under the terms of the CeCill
 ** Public License
 **-----------------------------------------------------------------------
 **
 ** File: call-via-socket.ml
 ** Author: erwan.jahier@univ-grenoble-alpes.fr
 *)

(* Launch prog and connect its stdin/stdout to sockets *)



let usage = "call-via-socket -addr <inet address> -port <port> [-serveur] \"<prog> <args>\"

Launch prog args connecting its stdin/stdout to a socket and stderr is to a log file.
Fails (with exit code 2) if the port is not available.
"


let client_mode = ref true
let inet_addr = ref (Unix.inet_addr_of_string "127.0.0.1")
let port = ref 2000
let usage_out speclist errmsg =
  Printf.printf "%s" (Arg.usage_string speclist errmsg)

let rec speclist =
  [
    "-addr", Arg.String(fun str -> inet_addr := Unix.inet_addr_of_string str),
    "<string>\tSocket inet address (127.0.0.1 by default)";

    "-port", Arg.Int(fun str -> port := str),
    "<int>\tSocket port (2000 by default)";

    "-server", Arg.Unit(fun () -> client_mode := false),
    "\tThe prog plays the role of the server (and the role if the client if unset)";

    "--help", Arg.Unit (fun _ -> (usage_out speclist usage ; exit 0)),
    "\tDisplay this list of options." ;
    "-help", Arg.Unit (fun _ -> (usage_out speclist usage ; exit 0)),
    "";
    "-h", Arg.Unit (fun _ -> (usage_out speclist usage ; exit 0)),
    ""
  ]


(* Parsing command line args *)
let prog, args =
  try 
    let prog = ref "" in
    let set_prog str = prog := !prog  ^ " " ^ str in
    let prog =
      Arg.parse speclist set_prog usage;
      (Str.split (Str.regexp "[ \t]+") !prog)
    in
      List.hd prog, Array.of_list prog
  with
    | Failure(e) -> output_string stdout e; flush_all(); exit 2
    | e -> output_string stdout (Printexc.to_string e); flush_all(); exit 2

let log_file = (prog ^ "-via-sockets-stderr.log")
let log = open_out log_file

let _ = 
  for i = 0 to Array.length Sys.argv -1 do 
    output_string log (Sys.argv.(i) ^ " ");
  done;
  output_string log "\n"; flush log;
  if Array.length Sys.argv < 3 then (
    print_string usage;
    flush stdout;
    close_out log;
    exit 2
  )

(*****************************************************************************)
(* Socket administration stuff *)

let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let inet_addr = !inet_addr
let inet_addr_str = Unix.string_of_inet_addr inet_addr
let port = !port

let rec connect_loop sock addr k =
  try Unix.connect sock addr
  with _ -> 
    if k > 0 then (
      output_string log "  call-via-socket: connect failed... try once more \n"; flush log;
      Unix.sleep 1;
      connect_loop sock addr (k-1)
    )
    else
      failwith "call-via-socket: cannot connect to the socket"

let (sock_in, sock_out) = 
  try
    if !client_mode then 
      (
        connect_loop sock (Unix.ADDR_INET(inet_addr, port)) 100 ;
        (* connect ne marche que si il y a un accept en attente coté
           serveur.  Cela entraine une course critique entre le serveur
           et le client. Pour y remédier, on essaie 10 fois en attendant
           une seconde à chaque essai. *)
        Printf.fprintf log "call-via-socket: sock connection on %s:%d succeeded " inet_addr_str port;
        (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
      )
    else
      ( (* Serveur mode *)
        Unix.bind sock (Unix.ADDR_INET(inet_addr, port));
        Unix.listen sock 1;
        let sock,_ = Unix.accept sock (* bloquant *) in
          Printf.fprintf log "call-via-socket -server:  sock connection on %s:%d accepted.\n" inet_addr_str port;
          (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
      )
  with 
      Unix.Unix_error(errcode, funcstr, paramstr) ->
	     output_string log "call-via-socket connect failure: ";
	     output_string log (Unix.error_message errcode);
	     output_string log ("(" ^ funcstr ^ " " ^ paramstr ^")\n");
	     flush log;
	     exit 2

(*****************************************************************************)
(* Forking *)
let pid = 
  output_string log ("call-via-socket "^prog^":");
  output_string log " create child process with '";
  for i = 0 to Array.length args -1 do 
    output_string log (args.(i)^ " ");
  done;
  output_string log "'\n";
  flush log;
  Unix.create_process prog args
    (Unix.descr_of_in_channel sock_in) 
    (Unix.descr_of_out_channel sock_out) 
    (Unix.descr_of_out_channel log)
 
let _ = 
  output_string log ("call-via-socket "^prog^": the process creation succeeded.\n");
  flush log;
  let _pid, pstatus = (Unix.waitpid [] pid) in
    (*   ignore(Unix.wait());  *)
    output_string log ("call-via-socket "^prog^":");
    (match pstatus with
         Unix.WEXITED i -> output_string log (
           " the process terminated with exit code " ^ (string_of_int i) ^"\n")
       | Unix.WSIGNALED i  -> output_string log (
           " the process was killed by signal " ^ (string_of_int i) ^"\n")
       | Unix.WSTOPPED  i  -> output_string log (
           " the process was stopped by signal " ^ (string_of_int i) ^"\n")
    );
    output_string log ("call-via-socket "^prog^": bye. \n");
    flush log;
    close_out log



