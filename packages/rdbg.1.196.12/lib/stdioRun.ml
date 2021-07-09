(* Time-stamp: <modified the 19/05/2021 (at 16:14) by Erwan Jahier> *)

open RdbgArg
 
type vars = (Data.ident * Data.t) list

(**********************************************************************************)

let debug_msg msg =     
  if args.debug_rdbg then (output_string stdout (msg^"\n"); flush stdout)


let (step_channel : string -> in_channel -> out_channel -> vars -> vars ->
     Data.subst list -> Data.subst list) =
  fun label ic oc in_vars out_vars sl ->
    let my_string_of_float v = Mypervasives.my_string_of_float v args.precision in
    let in_vals_str =
      List.fold_left
        (fun acc (name, _) ->
          try
            let value = List.assoc name sl in
            acc ^ " "^ (Data.val_to_rif_string my_string_of_float value)
          with Not_found -> acc
        )
        ""
        in_vars
    in
    let res =
      debug_msg  (label ^ " receives '" ^ in_vals_str ^"'.\n");
      output_string oc (in_vals_str ^"\n");
      flush oc;
      RifIO.read
        ~debug:args.debug_rdbg ~label:("read the result of "^label) ic None out_vars
    in
    res

open RdbgPlugin

let (make_gen : in_channel -> out_channel -> (string -> unit) -> string -> string ->
     RdbgPlugin.t) =
  fun cmd_ic cmd_oc kill ctx_id ctx_msg ->     
  let cmd_in, cmd_out = 
    try
      if args.debug_rdbg then (
        Printf.fprintf stderr "Wait for interface declarations on stdin (%s).\n" ctx_msg;
        flush stderr);
      flush cmd_oc;
      RifIO.read_interface ~debug:args.debug_rdbg ~label:ctx_msg cmd_ic (Some stderr)
    (* (if args.debug_rdbg then Some stderr else None) *)
    with RifIO.Bye -> exit 2
  in
  let step = 
    if args.debug_rdbg then (Printf.fprintf stderr "\nStep %s.\n" ctx_msg; flush stderr);
    step_channel ctx_id cmd_ic cmd_oc cmd_in cmd_out
  in
  let reset () =
    debug_msg (Printf.sprintf "==> send '#reset' to %s\n" ctx_msg) ;
    RifIO.write cmd_oc "#reset\n"; flush cmd_oc
  in 
  let step_dbg sl ctx cont =
    let cont2 sl_out ctx =
      { ctx with
        RdbgEvent.kind = RdbgEvent.Exit;
        RdbgEvent.lang = "RIF";
        RdbgEvent.sinfo = None;
        RdbgEvent.data = ctx.RdbgEvent.data @ sl_out;
        RdbgEvent.name=ctx_id;
        RdbgEvent.inputs = cmd_in;
        RdbgEvent.outputs = cmd_out;
        RdbgEvent.locals = [];
        RdbgEvent.next = (
          fun () ->
            let ctx = { ctx with
                        RdbgEvent.data = ctx.RdbgEvent.data @ sl_out;
                        RdbgEvent.nb = ctx.RdbgEvent.nb+1 } in
            cont sl_out ctx );
      }
    in
    { ctx with
      RdbgEvent.kind = RdbgEvent.Call;
      RdbgEvent.lang = "RIF";
      RdbgEvent.sinfo = None;
      RdbgEvent.name=ctx_id;
      RdbgEvent.inputs = cmd_in;
      RdbgEvent.outputs = cmd_out;
      RdbgEvent.locals = [];
      RdbgEvent.next = (
        fun () ->  
          let ctx = { ctx with RdbgEvent.nb = ctx.RdbgEvent.nb+1 } in
          cont2 (step sl) ctx );
    }
  in 
  {
    id = ctx_msg;
    inputs = cmd_in;
    outputs= cmd_out;
    reset=reset;
    kill=kill;
    save_state= (fun _ -> ());
    restore_state= (fun _ -> ());
    init_inputs=[];
    init_outputs=[];
    step=step;     
    step_dbg=step_dbg;
  }

let (make : string -> RdbgPlugin.t) =
  fun cmd -> 
  let (stdin_in,  stdin_out) =  Unix.pipe () in
  let (stdout_in, stdout_out) = Unix.pipe () in
  let cmd_ic = Unix.in_channel_of_descr stdout_in in
  let cmd_oc = Unix.out_channel_of_descr stdin_out in
  let _ =
    set_binary_mode_in  cmd_ic false;
    set_binary_mode_out cmd_oc false;
  in
  let arg_list = Str.split (Str.regexp "[ \t]+") cmd in
  let prog, _ = match arg_list with x::l -> x,l | [] -> assert false in
  
  (*   let label = Printf.sprintf "from %s" prog in *)
  let pid_cmd =
    let arg_array = Array.of_list arg_list in
    try 
      if args.debug_rdbg then (
        let msg = Printf.sprintf "Unix.create_process %s [%s]\n" 
                                 prog (String.concat "," arg_list)
        in
	     output_string stderr msg ;
        flush stderr
      );
      Unix.create_process prog arg_array stdin_in stdout_out Unix.stderr
    with Unix.Unix_error(e,_, prog) -> 
      let msg = Unix.error_message e in
      Printf.eprintf "*** Error when creating process with %s: %s\n" prog msg;
      exit 2
  in
  let _ =
    Printf.eprintf "Process %d (%s) created\n" pid_cmd cmd; flush stderr
  in
  let kill msg =
    (* Printf.print "EOF" *)
    Unix.close stdin_in;
    Unix.close stdin_out;
    Unix.close stdout_in;
    Unix.close stdout_out;
    (try 
        Printf.eprintf "%s\nKilling process %d (%s)\n" msg pid_cmd cmd;
        flush stderr;
        Unix.kill pid_cmd Sys.sigterm 
      with e ->
        Printf.printf "Killing %s process failed: %s\n" prog (Printexc.to_string e))
  in
  make_gen cmd_ic cmd_oc kill prog cmd
        
let rec connect_loop sock addr k =
  try Unix.connect sock addr
  with _ -> 
    if k > 0 
    then (
      if args.debug_rdbg then (
        let ni = Unix.getnameinfo addr [] in
          Printf.fprintf stderr "connect %s:%s failed;  try again in a second.\n" 
            ni.Unix.ni_hostname ni.Unix.ni_service; 
          flush stderr
      );
      Unix.sleep 1; 
      connect_loop sock addr (k-1) 
    )
    else failwith "rdbg: cannot connect to the socket"

let (make_socket : string -> int -> RdbgPlugin.t) =
  fun sock_adr port ->
    let _ =
      if args.debug_rdbg then (
        Printf.fprintf stderr "Start a connection on %s:%d\n" sock_adr port; 
        flush stderr)
    in
    let inet_addr = Unix.inet_addr_of_string sock_adr in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM  0 in
    let (sock_in, sock_out) =
      try
        connect_loop sock (Unix.ADDR_INET(inet_addr, port)) 10 ;
        if args.debug_rdbg then (
          Printf.fprintf stderr "Socket %s:%d connected \n" sock_adr port; 
          flush stderr);
        (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
      with 
          Unix.Unix_error(errcode, funcstr, paramstr) ->
            failwith ("rdbg socket connect failure: " ^ (Unix.error_message errcode) ^
                         "(" ^ funcstr ^ " " ^ paramstr ^")\n")
    in
    let kill msg =
      Printf.printf "Killing the socket process (%s:%i)\n" sock_adr port;
      print_string ("'"^msg^"'");
      flush stdout;
      output_string sock_out msg;
      flush sock_out;
      let str = input_line sock_in in
      (* make sure that the sut process has read the quit before closing socks *)
      print_string (str ^"\n");
      flush stdout;
      Unix.shutdown sock Unix.SHUTDOWN_ALL
    in
    let label = Printf.sprintf "[%s:%i] " sock_adr port in
  let _ =
    if args.debug_rdbg then (
      Printf.fprintf stderr "\nInterface declarations: ok.\n"; flush stderr);
  in
  make_gen sock_in sock_out kill label label
         
let (make_socket_init : string -> int -> RdbgPlugin.t) =
  fun _addr _port -> 
  assert false  (* finish me *)

(* exported *)
let (make_init : string -> RdbgPlugin.t) =
  fun _cmd -> 
  assert false (* finish me *)
(*     let sock_in, vars_in, vars_out, kill, step, step_dbg = make_socket_do sock_adr port in *)
(*     let out_init = RifIO.read ~debug:args.debug_rdbg sock_in None vars_out in *)
(*     let in_init = RifIO.read  ~debug:args.debug_rdbg sock_in None vars_in in *)
(*       vars_in, vars_out, kill, step, step_dbg, in_init, out_init *)
