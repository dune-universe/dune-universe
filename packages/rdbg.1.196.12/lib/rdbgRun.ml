(* Time-stamp: <modified the 05/07/2021 (at 14:57) by Erwan Jahier> *)

open Mypervasives
open RdbgArg
open RdbgPlugin

type vars = (Data.ident * Data.t) list
  
(* to be able to dump cov info if the exec is stopped by a ctrl-c. *)
let cov_ref = ref None
let gnuplot_pid_ref = ref None
let gnuplot_oc = ref None

(* Returns luciole io if necessary *)
let (check_compat : vars -> vars -> vars -> vars -> vars -> vars ->
     int * (vars * vars) option) =
  fun env_in env_out sut_in sut_out oracle_in _oracle_out -> 
  (* cf lurette.set_luciole_mode_if_necessary to add a call to luciole *)

  (* Extern type are viewed as string at the RIF level *)
  let externtostr (n,t) = n, (match t with Data.Extern _ -> Data.String | _ -> t) in
  let env_in = List.map externtostr env_in in
  let env_out = List.map externtostr env_out in
  let sut_in = List.map externtostr sut_in in
  let sut_out = List.map externtostr sut_out in
  let oracle_in = List.map externtostr oracle_in in

  let missing_sut_in = list_minus sut_in env_out
  and missing_env_in = list_minus  env_in sut_out
  and missing_oracle_in = list_minus oracle_in (sut_out @env_out)
  in
  let missing_out = list_union missing_sut_in missing_env_in in 
  let missing_in = list_minus (env_out@sut_out) missing_out in 
  (*     let missing_in = [] in  *)

  let vars_to_string vars = String.concat "," 
      (List.map (fun (n,t) -> n^":"^(Data.type_to_string t)) vars) 
  in
  if missing_sut_in <> [] then (
    let missing_str = vars_to_string missing_sut_in in
    Printf.eprintf "Some variables are missing in input of the SUT: %s\n" 
      missing_str;
    flush stderr
  ) ;
  if missing_env_in <> [] then (
    let missing_str = vars_to_string missing_env_in in
    Printf.eprintf "Some variables are missing in input of its environment: %s\n" 
      missing_str;
    flush stderr;
  );
  if missing_out <> [] then (
    0, Some(missing_in,missing_out)
  ) 
  else if missing_oracle_in <> [] then (
    let missing_str = vars_to_string missing_oracle_in in
    Printf.eprintf "*** Error: Some variables are missing in input of the oracle: %s\n" 
      missing_str;
    flush stderr;
    2,None
  ) 
  else (
    if List.mem ("Step") (fst(List.split missing_in)) then (
      Printf.eprintf 
        "*** You cannot use the name 'Step' for a variable with rdbg, sorry.\n";
      flush stderr;
      2,None
    ) else (
      Printf.eprintf "Variables are compatible.\n";
      flush stderr;
      0, None
    )
  )


type ctx = RdbgEvent.t
type e = RdbgEvent.t

let rec (list_split: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i* 'j) list -> 
         'a list * 'b list * 'c list * 'd list * 'e list * 'f list
         * 'g list * 'h list * 'i list* 'j list) = 
  function
    | [] -> ([], [], [], [], [], [], [], [], [], [])
    | (x,y,z,t,u,v,w,a,b,c)::l ->
      let (rx, ry, rz, rt, ru, rv, rw, ra, rb, rc) = list_split l in
      (x::rx, y::ry, z::rz, t::rt, u::ru, v::rv, w::rw, a::ra, b::rb, c::rc)

(***********************************************************************************)
(** Save and restore the rdbtop PRG state to support time-travel when the Random lib
 in used in rdbg commands *)
let rdbgtop_seeds = Hashtbl.create 10

let save_rdbgtop_state i =
  let prgs = Random.get_state () in
  if args.verbose > 0 then Printf.printf  "Save rdbgtop state %i (%i) \n%!" i
  (Random.int 10000);
  Random.set_state prgs;
  Hashtbl.replace rdbgtop_seeds i prgs

let restore_rdbgtop_state i =
  match Hashtbl.find_opt rdbgtop_seeds i with
  | Some prgs ->
     Random.set_state prgs;
     if args.verbose > 0 then Printf.printf  "Restore rdbgtop state %i (%i)\n%!"
                                i (Random.int 10000);
     Random.set_state prgs;
  | None -> Printf.eprintf "Warning: cannot restore state %i from rdbg\n%!" i

(***********************************************************************************)

let (make_rp_list : reactive_program list -> 
     vars list * vars list * (unit -> unit) list * (string -> unit) list *
     (int -> unit) list * (int -> unit) list *
       (Data.subst list -> Data.subst list) list * 
       (Data.subst list -> ctx -> (Data.subst list -> ctx -> e) -> e) list *
       Data.subst list list * Data.subst list list) =
  fun rpl -> 
    let aux rp = 
      let plugin =
        match rp with
          | Stdio(cmd)     -> StdioRun.make cmd 
          | StdioInit(cmd) -> StdioRun.make_init cmd 
          | Sock(addr,port)     -> StdioRun.make_socket addr port 
          | SockInit(addr,port) -> StdioRun.make_socket_init addr port 
          | Ocaml(plugin)  -> plugin
      in
      let ins, outs, reset, kill, save_state, restore_state, step, step_dbg,
          initin, initout =  
        plugin.inputs,plugin.outputs,plugin.reset,plugin.kill,plugin.save_state,
        plugin.restore_state,plugin.step,plugin.step_dbg,
        plugin.init_inputs,plugin.init_outputs
      in
      let step = if args.debug_rdbg then
          let (string_of_subst : Data.subst -> string) =
            fun (str, v) -> str ^ "<-" ^ (Data.val_to_string (string_of_float) v)
          in
          let sl2str sl = String.concat "," (List.map string_of_subst sl) in
          (fun sli ->
            let slo = step sli in
            Printf.eprintf "[%s] step(%s) = (%s) \n"
              (reactive_program_to_string rp) (sl2str sli) (sl2str slo);
            flush stderr;
            slo)
        else
          step
      in
      ins, outs, reset, kill, save_state, restore_state, step, step_dbg, initin, initout
    in
    list_split (List.map aux rpl)

let oracle_ok = ref true
let (rdbg_mv:  ((string * Data.t) list * (string * Data.t) list) ref) = ref ([],[])
let (rdbg_mv_hook : (Data.subst list -> Data.subst list option) option ref) = ref None

type cov_opt = 
    NO (* NoOracle *) 
  | OO (* OracleOnly *) 
  | OC of Coverage.t
exception OracleError of string

(* Transform a map on a function list into CPS *)
let (step_dbg_sl :
       ('s list -> 'ctx -> ('s list -> 'ctx -> 'e) -> 'e) list -> 
       's list -> 'ctx  -> ('ctx -> 's list -> 'e) -> 'e) = 
  fun step_dbg_sl_l sl ctx cont -> 
    (* ouch! Celle-la est chevelue...  
       La difficulté, c'est de passer un 'List.map step' en CPS.
       Suis-je aller au plus simple ? En tout cas j'ai réussit :)
    *)
    let rec (iter_step  : 
               ('s list -> 'ctx  -> ('s list -> 'ctx  -> 'e) -> 'e) list -> 
              ('ctx * 's list list) -> 's list -> 'e) = 
      fun stepl (ctx,res_stepl) sl ->
        match stepl with
          | [] -> cont ctx (List.flatten (res_stepl))
          | step::stepl ->
             step sl ctx (fun res_sl ctx -> 
                          iter_step stepl (ctx,(res_sl::res_stepl)) sl)
    in
      iter_step step_dbg_sl_l (ctx,[]) sl 


let (start : unit -> e) =
  fun () ->
  if args.verbose > 0 then (Printf.printf "# RdbgRun.start()\n"; flush stdout);
  let sut_in_l, sut_out_l, sut_reset_l, sut_kill_l, sut_ss_l, sut_rs_l,
      sut_step_sl_l, sut_step_dbg_sl_l, 
      sut_init_in_l, sut_init_out_l = make_rp_list args.suts 
  in
  let sut_reset () = List.iter (fun f-> f ()) sut_reset_l in
  let sut_save_state i = save_rdbgtop_state i; List.iter (fun f-> f i) sut_ss_l in
  let sut_restore_state i = restore_rdbgtop_state i; List.iter (fun f-> f i) sut_rs_l in
  let sut_kill msg = List.iter (fun f -> f msg) sut_kill_l in
  let sut_init_in = List.flatten sut_init_in_l in
  let sut_init_out = List.flatten sut_init_out_l in

  (* Get oracle info (var names, step func, etc.)*)
  let oracle_in_l, oracle_out_l, oracle_reset_l, oracle_kill_l, oracle_ss_l, oracle_rs_l,
      oracle_step_sl_l, oracle_step_dbg_sl_l, _, _ = make_rp_list args.oracles
  in
  let oracle_reset () = List.iter (fun f-> f ()) oracle_reset_l in
  let oracle_kill msg = List.iter (fun f -> f msg) oracle_kill_l in
  let oracle_save_state i = List.iter (fun f-> f i) oracle_ss_l in
  let oracle_restore_state i = List.iter (fun f-> f i) oracle_rs_l in

  (* Get env info (var names, step func, etc.)*)
  let env_in_l, env_out_l, env_reset_l, env_kill_l, env_ss_l, env_rs_l,
      env_step_sl_l, env_step_dbg_sl_l,
      env_init_in_l, env_init_out_l = make_rp_list args.envs 
  in
  let env_reset () = List.iter (fun f-> f ()) env_reset_l in
  let env_kill msg = List.iter (fun f -> f msg) env_kill_l in
  let env_save_state i = List.iter (fun f-> f i) env_ss_l in
  let env_restore_state i = List.iter (fun f-> f i) env_rs_l in
  let _env_init_in = list_rm_dup (List.flatten env_init_in_l) in
  let _env_init_out = list_rm_dup (List.flatten env_init_out_l) in

  let reset () =
    if args.verbose > 0 then (
      Printf.eprintf "rdbgRun.start: resetting all RPs\n"; flush stderr);
    sut_reset (); env_reset (); oracle_reset ()
  in
  let save_state i =
    sut_save_state i; env_save_state i; oracle_save_state i
  in
  let restore_state i =
    sut_restore_state i; env_restore_state i; oracle_restore_state i
  in
  let vars_to_string l = 
    String.concat "\n"
      (List.map (fun (vn,vt) ->
           Printf.sprintf "\t'%s':'%s'" vn (Data.type_to_string vt)) l)
  in
  let flat_sut_in  = list_rm_dup (List.flatten sut_in_l)
  and flat_sut_out = list_rm_dup (List.flatten sut_out_l)
  and flat_env_in  = list_rm_dup (List.flatten env_in_l)
  and flat_env_out = list_rm_dup (List.flatten env_out_l)
  and flat_oracle_in  = list_rm_dup (List.flatten oracle_in_l)
  and flat_oracle_out = list_rm_dup (List.flatten oracle_out_l)
  in
  let _ = if args.verbose > 0 || args.debug_rdbg then
      let sut_input_str = vars_to_string flat_sut_in in
      let sut_output_str = vars_to_string flat_sut_out in
      let env_input_str = vars_to_string  flat_env_in in
      let env_output_str = vars_to_string flat_env_out in
      let oracle_input_str = vars_to_string flat_oracle_in in
      let oracle_output_str_l = List.map vars_to_string oracle_out_l in
      Printf.printf "sut input : \n%s\n"  sut_input_str;
      Printf.printf "sut output : \n%s\n" sut_output_str;
      Printf.printf "env input : \n%s\n"  env_input_str;
      Printf.printf "env output : \n%s\n"  env_output_str;
      Printf.printf "oracle(s) input : \n%s\n"  oracle_input_str;
      List.iter (fun str -> Printf.printf "oracle output : \n%s\n" str)
        oracle_output_str_l;
      flush stdout
  in
  (* Check var names and types compat. *)
  let res_compat, missing_io_opt = 
    check_compat
      flat_env_in flat_env_out flat_sut_in flat_sut_out flat_oracle_in flat_oracle_out 
  in
  if res_compat>0 && not args.rdbg then
    exit res_compat;
  let (missing_kill, missing_step), missing_outputs_vars =
    match missing_io_opt with
    | None -> ((fun _ -> ()),(fun _ -> Some [])),[]
    | Some (missing_in, missing_out) ->
      rdbg_mv := (missing_in, missing_out);
      let missing_in_string =
        List.map (fun (n,t) -> n,Data.type_to_string t) missing_in
      and missing_out_string=
        List.map (fun (n,t) -> n,Data.type_to_string t) missing_out
      in
      (if args.luciole_mode then 
         (Printf.eprintf "read missing inputs from luciole\n";flush stderr;
          LucioleRun.make "./rdbg_luciole.dro" missing_in_string missing_out_string)
       else
         (Printf.eprintf "read missing inputs from stdin\n";flush stderr;
          RifRun.make missing_in_string missing_out_string)), 
      missing_out
  in
  let cov_init = (* XXX faut-il renommer les sorties de l'oracle 
                        ou raler en cas de clash ? *)
    if List.flatten oracle_out_l = [] then NO else 
      let oracle_out = List.flatten (List.map List.tl oracle_out_l) in
      if List.length oracle_out < 1 then OO else 
        let is_bool (_,t) = (t = Data.Bool) in
        let names = List.filter is_bool oracle_out in
        let names = fst (List.split names) in
        OC (Coverage.init names args.cov_file args.reset_cov_file)
  in 
  let oc = open_out args.output in
  let sim2chro_oc = if args.display_sim2chro then ExtTool.sim2chro_dyn () 
    else (open_out "/dev/null") in
  (* open_out "/dev/null" in *)
  let filter vals vars = 
    let vars = List.filter (fun (n,_) -> List.mem_assoc n vals) vars in
    List.map (fun (n,_t) -> n, List.assoc n vals) vars
  in
  let check_oracles oracle_in_vals i _oracle_out_l oracle_out_vals_l cov =
    let check_one_oracle = function 
      | [] -> assert false
      | (_, Data.B true)::tail -> tail

      | (_, Data.B false)::tail ->
        let msg = 
          match cov with 
            OC cov -> Coverage.dump_oracle_io oracle_in_vals tail cov 
          | _ -> ""
        in         
        let msg = Printf.sprintf 
            "\n*** An oracle returned false at step %i\n%s" i msg 
        in
        oracle_ok := false;
        if args.stop_on_oracle_error then raise (OracleError msg) else (
          Printf.printf "\027[35m %s \027[00m\n" msg;
          flush stdout;
          tail
        )
      | (vn, vv)::_  -> 
        let vv = Data.val_to_string_type vv in
        let msg = Printf.sprintf 
            "The oracle first output should be a bool; but %s has type %s"
            vn vv
        in
        failwith msg
    in
    match cov with 
      NO -> NO
    | OO -> ignore (List.map check_one_oracle oracle_out_vals_l); OO
    | OC cov -> 
      let ll = List.map check_one_oracle oracle_out_vals_l in
      let cov =
        List.fold_left
          (fun cov other_oracle_out_vals ->
             Coverage.update_cov other_oracle_out_vals cov) cov ll
      in
      cov_ref := Some cov;
      OC cov
  in
  let update_cov cov = 
    match cov with 
    | NO -> ()
    | OO -> ()
    | OC cov -> 
      let str =
        String.concat ", " (List.map reactive_program_to_string args.oracles)
      in
      Coverage.dump str args.output cov
  in
  let no_exc f str =
    try f str
    with e ->
      Printf.printf "kill failed: '%s'\n%!"  ((Printexc.to_string e))
  in
  let killem_all_do () =
    Printf.eprintf "Terminating all processes:\n" ; flush stderr;
    no_exc env_kill "q\n";
    no_exc sut_kill "q\n";
    no_exc missing_kill "q\n";
    no_exc oracle_kill "q\n";
    Printf.eprintf "Terminating all processes: done\n%!";
    close_out oc;
    close_out sim2chro_oc
  in
  let killem_all cov =
    killem_all_do ();
    update_cov cov   
  in
  let ctx_init =
    {
      RdbgEvent.nb = 1;
      RdbgEvent.step = 1;
      RdbgEvent.name = "rdbg";
      RdbgEvent.depth = 1;
      RdbgEvent.inputs = [];
      RdbgEvent.outputs = [];
      RdbgEvent.locals = [];
      RdbgEvent.data = [];
      RdbgEvent.terminate = (fun () -> killem_all cov_init);
      RdbgEvent.reset = (fun () -> reset ());
      RdbgEvent.save_state =  (fun i -> save_state i);
      RdbgEvent.restore_state = (fun i -> restore_state i);
      RdbgEvent.sinfo = None;
      (* fake values *)
      RdbgEvent.lang = "";
      RdbgEvent.next = (fun () -> assert false);
      RdbgEvent.kind = RdbgEvent.Ltop;
    }
  in
  let missing_step sl =
    (match !rdbg_mv_hook, !rdbg_mv with
     | None, _ -> missing_step sl
     | Some f, (_ins,outs) ->
       match f sl with
       | Some sl_out ->
         (* put vars in the right order *)
         Some (List.map (fun (n,_) ->
             n, (match List.assoc_opt n sl_out with
                 | Some x -> x
                 | None ->
                   failwith (Printf.sprintf
                               "rdbg_mv_hook do not provide all its output! (%s not in {%s})"
                               n (String.concat "," (fst (List.split sl_out)))))

           ) outs)
       | None -> None
    )
  in
  (* The main loop *)
  (* loop performs the missing_var step (if necessary), and then the env step 
     and calls loop2 *)
  let rec loop cov env_in_vals pre_env_out_vals missing_outs ctx () =
    if ctx.RdbgEvent.step > args.step_nb then (
      Printf.eprintf "Maximum step number reached\n" ; flush stderr;
      raise (RdbgEvent.End ctx.RdbgEvent.nb)
    ) 
    else
    if args.rdbg then 
      (* XXX l'idéal serait de faire ce test une seule fois à
                 l'exterieur de la boucle en passant la fonction qui va
                 bien selon le mode. Apres tout, c'est l'un des avantages
                 des CPS... Une autre solution serait de tout dupliquer,
                 mais bon, c'est mal.  *)

      if (args.missing_vars_at_the_end || !rdbg_mv = ([],[])) then (
        let edata = env_in_vals@pre_env_out_vals in
        let ctx = { ctx with data = edata ; depth = 1} in
        (* XXX duplic:step_dbg_sl *)
        let env_in_vals = List.rev_append missing_outs env_in_vals in
        let cont ctx = loop2 cov env_in_vals pre_env_out_vals ctx missing_outs in
        step_dbg_sl env_step_dbg_sl_l env_in_vals ctx cont
      )
      else
        let missing_step_in = env_in_vals@pre_env_out_vals in
        { ctx with
          RdbgEvent.name = "mv_hook";
          RdbgEvent.nb = ctx.nb;
          RdbgEvent.kind = RdbgEvent.Call;
          RdbgEvent.lang = "Ocaml";
          RdbgEvent.inputs  = fst !rdbg_mv; 
          RdbgEvent.outputs = snd !rdbg_mv;
          RdbgEvent.locals = [];
          RdbgEvent.depth  = 2;
          RdbgEvent.sinfo  = None;
          RdbgEvent.data = missing_step_in;
          RdbgEvent.next =
            (fun () ->
               let missing_outs_opt = missing_step missing_step_in in
               match missing_outs_opt with
               | None -> (* A '#reset' occurred: Start again with initial value *)
                 reset ();
                 loop cov sut_init_out sut_init_in missing_outs ctx_init ()
               | Some missing_outs -> (
                   let edata = missing_step_in @ missing_outs in
                   let ctx = { ctx with data = edata; nb = ctx.nb+1 ; depth = 1} in
                   (* XXX duplic:step_dbg_sl *)
                   { ctx with
                     RdbgEvent.name = "mv_hook";
                     RdbgEvent.nb = ctx.nb;
                     RdbgEvent.kind = RdbgEvent.Exit;
                     RdbgEvent.lang = "Ocaml";
                     RdbgEvent.inputs  = fst !rdbg_mv; 
                     RdbgEvent.outputs = snd !rdbg_mv;
                     RdbgEvent.locals = [];
                     RdbgEvent.depth  = 2;
                     RdbgEvent.sinfo  = None;
                     RdbgEvent.next =
                       (fun () ->
                          let env_in_vals = List.rev_append missing_outs env_in_vals in
                          let ctx = { ctx with nb = ctx.nb+1 } in
                          let cont ctx = loop2 cov env_in_vals pre_env_out_vals ctx missing_outs in
                          step_dbg_sl env_step_dbg_sl_l env_in_vals ctx cont
                       )
                   }
                 )
            )
        }
    else
      let missing_outs_opt =
        if (args.missing_vars_at_the_end )
        then Some missing_outs else missing_step (env_in_vals@pre_env_out_vals)
      in
      match missing_outs_opt with
      | None -> (* A '#reset' occurred: Start again with initial value *)
        reset ();
        loop cov sut_init_out sut_init_in missing_outs ctx_init ()
      | Some missing_outs -> (
          let env_in_vals =  List.rev_append missing_outs env_in_vals in
          let env_step_sl sl = List.flatten (List.map (fun f -> f sl) env_step_sl_l) in
          let env_out_vals =
            try  env_step_sl env_in_vals
            with e -> killem_all cov; raise e
          in
          loop2 cov env_in_vals pre_env_out_vals ctx missing_outs env_out_vals
        )
  (* loop2 performs the sut step and calls loop3 *)
  and loop2 cov _env_in_vals pre_env_out_vals ctx missing_outs env_out_vals =
    let env_out_vals = 
      try List.map (fun (v,_vt) -> v,List.assoc v env_out_vals) flat_env_out 
      with Not_found -> env_out_vals
    in
    let sut_in_vals = missing_outs @ env_out_vals in
    let sut_in_vals = filter sut_in_vals flat_sut_in in
    if args.rdbg then       
      let edata = sut_in_vals@ env_out_vals in
      let ctx = { ctx with RdbgEvent.name = "rdbg"; RdbgEvent.depth = 1;
                           RdbgEvent.data = edata } in
      let cont ctx =
        loop3 cov pre_env_out_vals env_out_vals missing_outs ctx
      in
      step_dbg_sl sut_step_dbg_sl_l sut_in_vals ctx cont
    else
      let sut_step_sl sl = List.flatten (List.map (fun f -> f sl) sut_step_sl_l) in
      let sut_out_vals =
        try sut_step_sl sut_in_vals
        with e -> killem_all cov; raise e
      in
      loop3 cov pre_env_out_vals env_out_vals missing_outs ctx sut_out_vals
  (* loop3 performs the mv step id needed, and then the oracle step and calls loop4 *)
  and loop3 cov pre_env_out_vals env_out_vals missing_outs ctx sut_out_vals =
    let sut_out_vals = 
      try List.map (fun (v,_vt) -> v,List.assoc v sut_out_vals) flat_sut_out 
      with Not_found -> sut_out_vals
    in
    let oracle_in_vals = 
      if args.delay_env_outputs 
      then List.rev_append pre_env_out_vals sut_out_vals 
      else List.rev_append     env_out_vals sut_out_vals 
    in

    if args.rdbg then
      let missing_step_in = env_out_vals@sut_out_vals in
      let oracle_in_vals = List.rev_append missing_outs oracle_in_vals in
      let oracle_in_vals = filter oracle_in_vals flat_oracle_in in
      if (not args.missing_vars_at_the_end || !rdbg_mv = ([],[])) then (
        let missing_outs =
          if args.missing_vars_at_the_end then
            (match missing_step  missing_step_in with
             | Some missing_outs -> missing_outs
             | None -> [] (* should not occur *)
            )
          else
            missing_outs
        in
        let edata = oracle_in_vals @missing_step_in @ctx.data in
        let ctx = { ctx with data=edata ; depth = 1} in
        let cont ctx =
          loop4 cov pre_env_out_vals env_out_vals ctx missing_outs sut_out_vals
            oracle_in_vals
        in
        step_dbg_sl oracle_step_dbg_sl_l oracle_in_vals ctx cont
      )
      else
        { ctx with
          RdbgEvent.name = "mv_hook";
          RdbgEvent.nb = ctx.nb;
          RdbgEvent.kind = RdbgEvent.Call;
          RdbgEvent.lang = "Ocaml";
          RdbgEvent.inputs  = fst !rdbg_mv;
          RdbgEvent.outputs = snd !rdbg_mv;
          RdbgEvent.locals = [];
          RdbgEvent.depth  = 2;
          RdbgEvent.sinfo  = None;
          RdbgEvent.data = missing_step_in;
          RdbgEvent.next =
            (fun () ->
               let missing_outs = (match missing_step  missing_step_in with
                   | Some missing_outs -> missing_outs
                   | None -> [] (* should not occur *)
                 )
               in
               let edata = oracle_in_vals @missing_step_in @missing_outs@ctx.data in
               let ctx = { ctx with data=edata;nb = ctx.nb+1 ; depth = 1} in
               { ctx with
                 RdbgEvent.name = "mv_hook";
                 RdbgEvent.nb = ctx.nb;
                 RdbgEvent.kind = RdbgEvent.Exit;
                 RdbgEvent.lang = "Ocaml";
                 RdbgEvent.inputs  = fst !rdbg_mv; 
                 RdbgEvent.outputs = snd !rdbg_mv;
                 RdbgEvent.locals = [];
                 RdbgEvent.depth  = 2;
                 RdbgEvent.sinfo  = None;
                 RdbgEvent.next =
                   (fun () ->
                      let cont ctx =
                        loop4 cov pre_env_out_vals env_out_vals ctx missing_outs sut_out_vals
                          oracle_in_vals
                      in
                      let ctx = { ctx with nb = ctx.nb+1 } in
                      step_dbg_sl oracle_step_dbg_sl_l oracle_in_vals ctx cont
                   )
               }
            )
        }
    else
      let missing_outs =
        if args.missing_vars_at_the_end then
          (match missing_step (env_out_vals@sut_out_vals) with
           | Some missing_outs -> missing_outs
           | None -> [] (* should not occur *)
          )
        else
          missing_outs
      in
      let oracle_in_vals = List.rev_append missing_outs oracle_in_vals in
      let oracle_in_vals = filter oracle_in_vals flat_oracle_in in
      let oracle_out_vals_l : Data.subst list =
        try List.flatten (List.map (fun f -> f oracle_in_vals) oracle_step_sl_l)
        with e -> killem_all cov; raise e
      in
      (* let oracle_out_vals = List.flatten oracle_out_vals_l in *)
      loop4 cov pre_env_out_vals env_out_vals ctx missing_outs sut_out_vals
        oracle_in_vals oracle_out_vals_l 

  (* loop4 terminates the step (outputs rif, checks oracles) and calls loop *)
  and loop4 cov pre_env_out_vals env_out_vals ctx missing_outs sut_out_vals
      oracle_in_vals oracle_out_vals_l =
    let oracle_out_vals_l = 
      try List.map 
            (fun oracle_out -> 
               List.map (fun (v,_vt) -> v,List.assoc v oracle_out_vals_l) oracle_out) 
            oracle_out_l
      with Not_found -> [oracle_out_vals_l] (* dead code? *)
    in
    let my_string_of_float v = Mypervasives.my_string_of_float v args.precision in
    let print_val (_vn,vv) = Data.val_to_rif_string my_string_of_float vv  in
    (*       Printf.fprintf oc "#step %d\n" i; *)
    if not args.no_rif then (
      output_string oc ("#step "^(string_of_int ctx.RdbgEvent.step)^"\n");
      if args.delay_env_outputs then (
        output_string oc (String.concat " " (List.map print_val (pre_env_out_vals)));
        output_string sim2chro_oc 
          (String.concat " " (List.map print_val (pre_env_out_vals)));
      )
      else (
        let missing_outs_str = String.concat " " (List.map print_val missing_outs) in
        let env_out_vals_str = String.concat " " (List.map print_val env_out_vals) in
        if args.missing_vars_at_the_end  then (
          output_string oc env_out_vals_str;
          output_string sim2chro_oc env_out_vals_str;
          if env_out_vals_str <> "" then (
            output_string oc " ";
            output_string sim2chro_oc " "
          );
          output_string oc missing_outs_str;
          output_string sim2chro_oc missing_outs_str;
          if missing_outs_str <> "" then (
            output_string oc " ";
            output_string sim2chro_oc " "
          );
        )
        else (
          output_string oc missing_outs_str;
          output_string sim2chro_oc missing_outs_str;
          if missing_outs_str <> "" then (
            output_string oc " ";
            output_string sim2chro_oc " "
          );
          output_string oc env_out_vals_str;
          output_string sim2chro_oc env_out_vals_str;
          if env_out_vals_str <> "" then (
            output_string oc " ";
            output_string sim2chro_oc " "
          );
        )
      );
      output_string oc "#outs ";
      output_string oc (String.concat " " (List.map print_val sut_out_vals));
      output_string oc "\n";
      List.iter (fun l -> 
          output_string oc "#oracle_outs ";
          output_string oc (String.concat " " (List.map print_val l));
          output_string oc "\n";
        ) oracle_out_vals_l;
      flush oc;

      output_string sim2chro_oc "#outs ";
      output_string sim2chro_oc (String.concat " " (List.map print_val sut_out_vals));
      output_string sim2chro_oc "\n";
      flush sim2chro_oc;
    );
    if (not args.go) && args.display_gnuplot then (
      (match !gnuplot_oc with
       | None ->  
         let oc, pid = 
           GnuplotRif.terminal := GnuplotRif.Wxt;
           GnuplotRif.verbose := args.verbose>1;
           GnuplotRif.dynamic := true;
           GnuplotRif.rif_file := args.output;
           GnuplotRif.f ()
         in
         gnuplot_pid_ref := Some pid;
         gnuplot_oc := Some oc
       | Some oc -> output_string oc "replot\n"; flush oc)
    ) else (
    );
    let step_nb = ctx.RdbgEvent.step in
    let en = ctx.RdbgEvent.nb in
    let edata = missing_outs@sut_out_vals@env_out_vals@(List.flatten oracle_out_vals_l) in
    let term () = 
      (match !gnuplot_pid_ref with
       | None -> ()
       | Some pid ->
         print_string "Killing gnuplot...\n"; flush stdout;
         Unix.kill pid Sys.sigkill;
         gnuplot_oc := None;
         gnuplot_pid_ref := None); 
      killem_all cov
    in
    let ctx = { ctx with 
                RdbgEvent.nb = en+1;
                RdbgEvent.step = step_nb+1;
                RdbgEvent.name = "rdbg";
                RdbgEvent.depth = 1;
                RdbgEvent.data = edata;
                RdbgEvent.terminate = term;
              }
    in
    if args.rdbg then (
      { ctx with 
        RdbgEvent.step = step_nb;
        RdbgEvent.nb = en;
        RdbgEvent.kind = RdbgEvent.Ltop;
        RdbgEvent.lang = "";
        RdbgEvent.inputs  = [];
        RdbgEvent.outputs = [];
        RdbgEvent.locals = [];
        RdbgEvent.sinfo  = None;
        RdbgEvent.next =
          (fun () ->
             loop (check_oracles oracle_in_vals step_nb oracle_out_l
                     oracle_out_vals_l cov )
               sut_out_vals env_out_vals missing_outs ctx ()
          );
      }
    )
    else (* lurette mode *)
      loop 
        (check_oracles oracle_in_vals step_nb oracle_out_l oracle_out_vals_l cov)
        sut_out_vals env_out_vals missing_outs ctx ()
  in
  (* end loop4 *)
  let loc = None in
  let dump_rp lbl rp =
    Printf.sprintf "# %s: %s\n" lbl (reactive_program_to_string rp)
  in
  let _ =
    RifIO.write oc ("# Rdbg Version \"" ^ RdbgVersion.str ^ "\" (\"" ^
                    RdbgVersion.sha^"\")\n");

    List.iter (fun rp  -> RifIO.write oc (dump_rp "sut" rp)) args.suts;
    List.iter (fun rp  -> RifIO.write oc (dump_rp "env" rp)) args.envs;
    List.iter (fun rp  -> RifIO.write oc (dump_rp "oracle" rp)) args.oracles;

    if not args.no_rif then (
      let sut_in = 
        if args.missing_vars_at_the_end then 
          flat_env_out@missing_outputs_vars
        else
          missing_outputs_vars@flat_env_out
      in
      RifIO.write_interface oc sut_in flat_sut_out loc (Some oracle_out_l);
      RifIO.flush oc;
      RifIO.write_interface sim2chro_oc sut_in flat_sut_out loc (Some oracle_out_l);
      RifIO.flush sim2chro_oc;
    )
  in
  let (first_event : e) =
    let res = 
      try
        if res_compat = 0 then
          loop cov_init sut_init_out sut_init_in [] ctx_init ()
        else
          raise(RdbgEvent.End res_compat) 
      with
      | RifIO.Bye ->
        killem_all_do ();
        raise RifIO.Bye
      | OracleError str ->
        Printf.printf "\027[35m %s \027[00m\n"  str;
        flush stdout;
        killem_all_do ();
        raise(RdbgEvent.End 1)

      | Failure str ->
        if str = "Normal termination" then (
          Printf.printf "\027[35m %s \027[00m\n" "Normal termination";
          flush stdout;
          killem_all_do ();
          raise(RdbgEvent.End 0)
        )
        else (
          print_string ("Failure: "^str^ " ");
          flush stdout;
          killem_all_do ();
          raise(RdbgEvent.End 1)
        )
      | RdbgEvent.End i ->
         Printf.printf "rdbg stops at event %d\n%!" i;
         killem_all_do ();
         raise(RdbgEvent.End i)
      | e -> 
        print_string ((Printexc.to_string e));
        flush stdout;
        killem_all_do ();
        raise(RdbgEvent.End 1)
    in
    res
  in
  first_event

    
(* exported *)
let (clean_terminate : int -> unit) =
  fun exit_code ->
    let str = String.concat ", " (List.map reactive_program_to_string args.oracles) in
    (match !gnuplot_pid_ref with
     | None -> ()
     | Some pid ->
        print_string "Killing gnuplot...\n"; 
        flush stdout;
        Unix.kill pid Sys.sigkill;
        gnuplot_pid_ref := None
    );
    (match !cov_ref with 
      | None -> ()
      | Some cov ->
         let (to_cov, covered, cov_rate)= Coverage.compute_stat cov in
         Coverage.dump str args.output cov;
         cov_ref:=None;
         if not !oracle_ok then () else
           if to_cov = covered then
             print_string "\027[35m No oracle was violated and all of them were covered.
 Congratulations! \027[00m\n%!"
           else (
             print_string "\027[35m No oracle was violated. ";
             Printf.printf "\n The coverage rate is %.1f%s " cov_rate "%";
             Printf.printf "\n More information on coverage in file '%s' \027[00m\n"
                           (Coverage.get_file_name cov);
           );
         flush stdout
    );
    if not !oracle_ok then (
      print_string "\027[35mOuch: an oracle was violated. ";
      print_string "exiting with error 2.\027[00m\n";
      flush stdout;
      exit 2)
    else
      exit exit_code

let (lurette_start : unit -> unit) =
  fun () -> 
  ignore(start ());
  clean_terminate 0

    
