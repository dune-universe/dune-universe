(* Time-stamp: <modified the 23/07/2020 (at 11:48) by Erwan Jahier> *)


type vars = (string * string) list
type sl = Data.subst list

open RdbgArg

let debug_msg msg =     
  if args.debug_rdbg then (output_string stdout ("LucioleRun: "^msg); flush stdout)


let myexit i = 
  if args.rdbg then failwith "error when calling luciole" else exit i

let output_msg msg = output_string stdout msg; flush stdout

let first_reset = ref true

let (interpret_pragma: string -> unit) =
  fun str ->
    match Mypervasives.string_to_string_list str with
    | "#q"::_ | "#quit"::_ -> 
      debug_msg ("luciole sent #quit\n");
      (* XXX raise a specific exc instead? *) 
      failwith "luciole process has terminated"
    | "#reset"::_ ->
      debug_msg ("luciole sent #reset \n");
      if !first_reset then first_reset := false else raise RifIO.Reset
    | _ -> 
      debug_msg ("Skipping luciole comments (" ^ str ^ ")\n")

let (make : string -> vars -> vars ->  (string -> unit) * (sl -> sl option)) =
  fun dro_file luciole_inputs luciole_outputs -> 
    if luciole_outputs <> ["Step","bool"] || luciole_outputs <> [] then (
      Printf.eprintf "Inputs are missing. Try to generate them with luciole\n"; 
      Printf.eprintf "Luciole: generate rdbg_luciole.c\n"
    ); 
    Luciole.gen_stubs ~boot:(not args.missing_vars_at_the_end) "rdbg" luciole_outputs luciole_inputs;
    Printf.eprintf "Luciole: generate rdbg_luciole.dro from rdbg_luciole.c\n"; 
    flush stderr;
    if RdbgMisc.c2dro "rdbg_luciole.c" then () else 
      ( 
        output_msg "*** Rdbg: Fail to generate rdbg_luciole.dro for luciole!\n"; 
        myexit 2
      );    

    Printf.eprintf "\nluciole: launch simec_trap on rdbg_luciole.dro\n"; 
    let (luciole_stdin_in,  luciole_stdin_out ) = Unix.pipe () in
    let (luciole_stdout_in, luciole_stdout_out) = Unix.pipe () in

    let luciole_ic = Unix.in_channel_of_descr  luciole_stdout_in in
    let luciole_oc = Unix.out_channel_of_descr luciole_stdin_out in
    let _ = 
      if Sys.os_type <> "Win32" then Unix.set_nonblock luciole_stdin_out;
      if Sys.os_type <> "Win32" then Unix.set_nonblock luciole_stdout_out;
      set_binary_mode_in  luciole_ic false;
      set_binary_mode_out luciole_oc false;
    in
    let prog = "simec_trap" ^ (if Sys.os_type="Win32" then ".bat" else "") in
    let luciole_args = [dro_file; string_of_int (Unix.getpid())] in
    let pid = 
      match Mypervasives.my_create_process 
              ~std_in:luciole_stdin_in ~std_out:luciole_stdout_out
              ~wait:false
              prog
              luciole_args
      with
      | Mypervasives.KO -> failwith ("error when calling simec_trap" ^ dro_file);
      | Mypervasives.OK -> assert false
      | Mypervasives.PID pid -> 
        debug_msg (prog ^ " " ^ dro_file ^ ": ok\n"); 
        pid
    in
    let kill msg = 
      debug_msg "kill the luciole process\n";
      output_string luciole_oc "q\n";
      flush luciole_oc;
      close_out luciole_oc;
      close_in luciole_ic;
      (try 
         (* if ever the 'q' did not kill the luciole process *)
         Printf.eprintf "%s\nKilling simec_trap (luciole) process %d\n" msg pid;
         flush stderr;
         Unix.kill pid Sys.sigkill
       with e -> (Printf.printf "Killing of luciole process failed: %s\n"
                    (Printexc.to_string e) ))
    in
    let (step : Data.subst list -> Data.subst list option) = 
      fun sl -> 
        (* Sends values to luciole *)
        List.iter
          (fun (n,_t) -> 
             try
               let value = List.assoc n sl in
               let sof v = Mypervasives.my_string_of_float v args.precision in
               let val_str = (Data.val_to_rif_string sof value) ^"\n" in
               if args.debug_rdbg then
                 Printf.fprintf stdout "send Luciole the value of %s: '%s'" n val_str;
               output_string luciole_oc val_str
             with Not_found -> 
               ()
          )
          luciole_inputs;
        flush luciole_oc;

        debug_msg "Start reading Luciole outputs...\n";
        (* Reads values from luciole *)
        try 
          let sl_out =
            List.map 
              (fun (name, vtype) -> 
                 let str = 
                   let rstr = ref (input_line luciole_ic) in
                   while String.length !rstr = 0 || String.sub !rstr 0 1 = "#" do
                     if String.sub !rstr 0 1 = "#" then interpret_pragma !rstr;
                     rstr := input_line luciole_ic
                   done;
                   !rstr
                 in
                 debug_msg ("luciole.step produced a value for "^name^":'"^str^"'\n");
                  let value = 
                   match vtype,str with
                   | "bool","t" -> Data.B(true) 
                   | "bool","f" -> Data.B(false) 
                   | "bool",_ -> ( 
                       output_msg ("luciole.step: Can not convert the value of "
                                   ^name^" into a bool:'"^str^"'\n");
                       myexit 2 
                     )
                   | "int",_ -> (
                       try Data.I(int_of_string str)
                       with e ->  
                         output_msg ("luciole.step: Can not convert the value of "^
                                     name^" into an int:'"^str^"'\n"^
                                     (Printexc.to_string e));
                         myexit 2
                     )
                   | "real",_ -> (
                       try Data.F(float_of_string str)
                       with e ->  
                         output_msg ("luciole.step: Can not convert  the value of "
                                     ^name^" into a float:'"^str^"'\n"^
                                     (Printexc.to_string e));
                         myexit 2)
                   |  _,_ -> assert false
                 in
                 (name, value)
              )
              luciole_outputs
          in
          debug_msg "end of luciole.step.\n";
          Some sl_out
        with RifIO.Reset -> None
    in
    kill, step
