(*pp  camlp4o pa_macro.cmo -USTAT *)

(** LUTIN2 : programme principal *)
open MainArg
open LutErrors

exception Stop

(* selection du fichier de sortie *)
let outchannel nodename opt = (
   if (MainArg.outpipe opt) then (
      stdout
   ) else (
      let outname = (
         match MainArg.outfile opt with
            Some outf -> outf
         |  None -> (
            let inbase = Filename.chop_extension (
               Filename.basename (List.hd (MainArg.infile opt))
            ) in
            (inbase)^"-"^(nodename)^".luc"
         )
      ) in
Verbose.put "#OUTFILE: %s\n" outname;
      open_out outname
   )
)

let to_luc mainprg tlenv opt =  (
   (* --> la fct qui traite un noeud *)
   let compile_node mnode = (
      Verbose.put "#COMPILING %s\n" mnode;
      let exped = Expand.make tlenv mainprg mnode in
      if (MainArg.test_expand opt) then (
         Verbose.put "#EXPANDING %s\n" mnode;
         Expand.dump exped
      ) else (
         Verbose.put "#AUTOGEN %s\n" mnode;
         (* let auto = AutoGen.make exped in *)
         let auto = AutoGen.make exped in
         let srcname =
            if (MainArg.infile opt = []) then ["stdin"] else (MainArg.infile opt)
         in
         if (MainArg.test_auto opt) then (
            (* AutoGen.dump auto *)
            AutoGen.dump auto
         ) else (
            (* génération d'un fichier lucky *)
            let oc = outchannel mnode opt in 
              List.iter (fun f -> Auto2Lucky.make f mnode auto oc) srcname
         )
      )
   ) in
   (* --> recherche du ou des mains *)
   let nodearg = MainArg.main_node opt in
   if (nodearg = "") then (
     (* expanse et compile tous les "mains" *)
     let nl = (Syntaxe.pack_node_list mainprg) in
     let pn n = (
       Verbose.put "#NODE %s\n" n
     ) in
       List.iter pn nl ;
       
       List.iter compile_node
         (Syntaxe.pack_node_list mainprg)
   ) else (
     compile_node nodearg
   )
)

(* debug *)
(*
  initial_ctrl_state : ctrl_state list;
  in_vars  : Exp.var list;
  out_vars : Exp.var list;
  loc_vars : Exp.var list;
  ext_func_tbl : Exp.ext_func_tbl;
  memories_names   : (string * Exp.var) list ; (* redundant, but useful *)
  bool_vars_to_gen : Exp.var list list ;
  num_vars_to_gen  : Exp.var list list ;
  output_var_names : Var.name list list ; (* redundant, but useful *)
  reactive : bool ; (* A scorie to remove. At least, it should be a list of bool *)

*)

let dump_var v = (
   Printf.fprintf stderr "  %s\n" (Var.to_string v)
)
let dump_prog p = (
   Printf.fprintf stderr "INPUTS:\n";
   List.iter dump_var p.Prog.in_vars;  
   Printf.fprintf stderr "OUTPUTS:\n";
   List.iter dump_var p.Prog.out_vars; 
   Printf.fprintf stderr "LOCALS:\n";
   List.iter dump_var p.Prog.loc_vars; 
   Printf.fprintf stderr "bool_vars_to_gen:\n";
   List.iter dump_var (List.flatten p.Prog.bool_vars_to_gen);  
   Printf.fprintf stderr "\n";
   Printf.fprintf stderr "num_vars_to_gen:\n";
   List.iter dump_var (List.flatten p.Prog.num_vars_to_gen);   
   Printf.fprintf stderr "\n";
   Printf.fprintf stderr "memories_names:\n";
   List.iter (fun (n,v) -> Printf.fprintf stderr "%s / %s ; " n (Var.to_string v)) p.Prog.memories_names;
   Printf.fprintf stderr "\n";
)

let gnuplot_pid_ref = ref None
let gnuplot_oc = ref None

(* simu : old lutin main XXX obselete !*)
let rec to_simu oc infile mnode opt = (
  (* Parse and build the internal structure *)
  let (zelutprog, zeprog) = LutProg.make ~libs:(MainArg.libs opt) infile mnode in 
  (* zelutprog is necessary for extracting initial data store *)
  let state0 = LutProg.get_init_state zelutprog zeprog in
  let init_state_dyn = { state0.Prog.d with  Prog.verbose = Verbose.level () } in
  let init_state = { state0 with Prog.d = init_state_dyn } in
  let loc = if (MainArg.show_locals opt) then
      Some init_state.Prog.s.Prog.loc_vars else None
  in
  let seed = MainArg.seed opt in
  let msg =
    Printf.sprintf
      "# This is lutin Version %s (\"%s\")\n# The random engine was initialized with the seed %d\n"
      Version.str Version.sha seed
  in
  let noo = not (MainArg.only_outputs opt) in
  Random.init seed;

  Rif.write oc msg;
  Rif.write_interface oc init_state.Prog.s.Prog.in_vars
    init_state.Prog.s.Prog.out_vars loc None;
  Rif.flush oc;
  let next_state = init_state in
  let lut_memories =
    if MainArg.load_mem opt then (
      output_string oc "#lutin_outputs_memories ";
      Rif.read (Verbose.level()>0) stdin (if noo then Some oc else None)
        init_state.Prog.s.Prog.out_vars)
    else
      Value.OfIdent.empty
  in
  (* first main_loop : no pre context *)
  main_loop oc opt 1 next_state lut_memories next_state lut_memories
)

and main_loop oc opt t init_state init_pre_lut_output state pre_lut_output = (
  Verbose.exe ( fun () ->
    let csnme = (Prog.ctrl_state_to_string_long state.Prog.d.Prog.ctrl_state) in
    Verbose.put "#Main.main_loop ctrl_state=%s\n" csnme;
  );
  if state.Prog.s.Prog.is_final state.Prog.d.Prog.ctrl_state 
  then () 
  else
    try
      main_loop_core oc opt t init_state init_pre_lut_output state pre_lut_output
    with RifIO.Reset ->
      main_loop_core oc opt t init_state init_pre_lut_output init_state init_pre_lut_output
)
and main_loop_core oc opt t init_state init_pre_lut_output  state _pre_lut_output = (
  let step_str =
    ("#step " ^ (string_of_int t)) ^
      (if state.Prog.d.Prog.verbose >= 1
       then (" (" ^  (Prog.ctrl_state_to_string_long state.Prog.d.Prog.ctrl_state) ^ "):")
       else ""
      ) ^ "\n" 
  in
  let noo = not (MainArg.only_outputs opt) in
  let _ = if noo then Rif.write oc step_str in
  let lut_input = if t=1 && (boot opt) then Value.OfIdent.empty else
        Rif.read (Verbose.level()>0) stdin (if noo then Some oc else None)
          state.Prog.s.Prog.in_vars     
  in
  let state_ref = ref state in
  let generator = LucFGen.get lut_input state_ref in
  
  (*
    Call Lucky explorer/solver
    env_step : step_mode -> Var.env_in -> Prog.state -> FGen.t list -> Prog.state * solution
    where type solution = Var.env_out * Var.env_loc
  *)
  let (next_state, (lut_output, loc)) = try (
    let sol = Lucky.env_step (MainArg.step_mode opt) lut_input state_ref generator in
    !state_ref, sol
    
  ) with
  | FGen.NoMoreFormula ->
    let state = !state_ref in
      Rif.write oc ("# " ^ (Prog.ctrl_state_to_string_long state.Prog.d.Prog.ctrl_state));
      Rif.write oc "# No transition is labelled by a satisfiable formula.\n" ;
      Rif.write oc "# The Lutin program is blocked.\n";
      Rif.flush oc;
      if state.Prog.s.Prog.is_final state.Prog.d.Prog.ctrl_state
      then exit 0
      else exit 2
    | FGen.NormalStop msg -> (
      (if (msg = "vanish")
       then Rif.write oc "# Simulation ended normally.\n"
       else Rif.write oc ("# Simulation ended with uncaught exception \""^msg^"\"\n")
      );
      Rif.flush oc;
      exit 0
    )
  in
  Verbose.exe ~level:3
    (fun () -> Printf.fprintf oc "#OUT SUBST LIST=%s\n" (Value.OfIdent.to_string ";" lut_output));
  Verbose.exe ~level:3
    (fun () -> Printf.fprintf oc "#LOC SUBST LIST=%s\n" (Value.OfIdent.to_string ";" loc));
  
  if noo then Rif.write oc " #outs ";
  Rif.write_outputs oc next_state.Prog.s.Prog.out_vars lut_output;

  if noo && MainArg.show_locals opt then (
    Rif.write oc "\n#locs ";
    Rif.write_outputs oc next_state.Prog.s.Prog.loc_vars loc;
  );

  Rif.write oc "\n";
  Rif.flush oc;

  (* Clean-up cached info that depend on pre or inputs *)
  let next_state = { state with
                     d = { next_state.d with snt = Bddd.clear next_state.d.snt } }
  in

  match MainArg.max_steps opt with
  |  None -> main_loop oc opt (t+1) init_state init_pre_lut_output
               next_state lut_output 
    |  Some max -> (
      if (t < max) then (
             (* next pre's are current vals  *)
             (* let next_state = LutProg.memorize_data next_state lut_input lut_output loc in *)
        main_loop oc opt(t+1) init_state init_pre_lut_output next_state lut_output 
      ) else (
        if noo then Rif.write oc "\n#end\n" else Rif.write oc "\nq\n"; 
        Rif.flush oc)
    )
)


let dbgexe = Verbose.get_flag "LutExe"

let quit oc = Rif.write oc "q\n"; Rif.flush oc


(* simu with LutExe *)
let to_exe oc infile mnode opt = (
  let exe = LutExe.make opt  infile mnode in

  Verbose.exe ~flag:dbgexe (fun _ -> LutExe.dump exe);
  let in_vars = LutExe.in_var_list exe in
  let out_vars = LutExe.out_var_list exe in
  let loc_vars = LutExe.loc_var_list exe in

  let do_stop = match MainArg.max_steps opt with
    | None -> (fun _ -> false)
    | Some max -> (fun c -> (c >= max))
  in

  let noo = not (MainArg.only_outputs opt) in
  let rec do_step cpt init_ctrl init_ins init_pres ctrl ins pres = (

    (* Clean-up cached info that depend on pre or inputs *)
    (*     let exe = LutExe.clear exe in *)
    Verbose.put "#Main.to_exe: step %d\n" cpt;
    let bg = LutExe.get_behavior_gen exe ins pres ctrl in
    match bg () with
      | _, LutExe.NoMoreBehavior i -> ( 
        Rif.write oc ("#end.\n# Simulation ended with uncaught Deadlock " ^
                         (if i=0 then "" else ("at event " ^ (string_of_int i)))^ "\n");
        Rif.flush oc;
        exit 2
      )
      | exe, LutExe.SomeBehavior (b, _bg') -> (
        match b with
          | LutExe.Goto (zeguard, ctrl') -> (
            (* THIS IS THE NORMAL BEHAVIOR *)
            MainArg.event_incr opt;
            let _exe, zeguard, (outs, locs) = try LutExe.find_one_sol exe zeguard
              with Not_found -> assert false
            in
            (* Try to display more useful source level info with -vl
               1.  *)
            if (Verbose.level() = 1) then (
              Printf.eprintf ("\n --> %s\n ") 
                (LutExe.guard_to_string zeguard);
              flush stderr
            );
            
            if noo then Rif.write oc (if in_vars = [] then "#outs " else " #outs ");
            Rif.write_outputs oc out_vars outs ;
            if noo && MainArg.show_locals opt then (
              Rif.write oc "\n#locs ";
              Rif.write_outputs oc loc_vars locs
            );
            Rif.write oc "\n";
            Rif.flush oc;
            (match call_gnuplot opt, riffile opt with
              | true, Some file -> 
                if cpt = 1 then (
                  let oc, pid = 
                    GnuplotRif.terminal := GnuplotRif.Wxt;
                    GnuplotRif.verbose := Verbose.level()>1;
                    GnuplotRif.dynamic := true;
                    GnuplotRif.rif_file := file;
                    GnuplotRif.f ()
                  in
                  gnuplot_pid_ref := Some pid;
                  gnuplot_oc := Some oc
                ) 
                else 
                  (match !gnuplot_oc with
                    | None -> ()
                    | Some oc -> output_string oc "replot\n"; flush oc)
              | _,_ -> ()
            );
            (* prepare next step *)
            if do_stop cpt then (
              if noo then ( 
                Rif.write oc "#end. Max step number reached.\n";
                Rif.flush oc
              ) else quit oc;
              exit 0
            ) else (
              let step_str = Printf.sprintf "#step %d\n" (cpt+1) in
              if noo then (
                Rif.write oc step_str;
                Rif.flush oc
              );
              try 
                let pres' = LutExe.make_pre ins outs locs in
                let ins' = Rif.read (Verbose.level()>0) stdin
                    (if noo then Some oc else None) in_vars in
                MainArg.event_incr opt; 
                do_step (cpt+1) init_ctrl init_ins init_pres ctrl' ins' pres'
              with RifIO.Reset ->
                do_step (cpt+1) init_ctrl init_ins init_pres  init_ctrl init_ins init_pres 
            )
          )
          | LutExe.Raise x -> (
            Rif.write
              oc ("\n#end. Simulation ended with uncaught user exception \""^x^"\"\n");
            quit oc;
            Rif.flush oc;
            exit 2
          )
          | LutExe.Vanish -> (
            if noo then ( 
              Rif.write oc "\n#end. Simulation ended normally.\n";
              Rif.flush oc
            ) else quit oc;
            exit 0
          )
      )
  ) 
  in  
  let seed = MainArg.seed opt in

  let msg =
    Printf.sprintf
      "# This is lutin Version %s (\"%s\")\n# The random engine was initialized with the seed %d\n"
      Version.str Version.sha seed
  in
  Random.init seed;
  Rif.write oc msg;
  Rif.write_interface oc in_vars out_vars
    (if  (MainArg.show_locals opt) then Some loc_vars else None) None;
  Rif.flush oc;

  (* prepare first step *)
  Verbose.put "#Main.to_exe: prepare first step\n";
  let ctrl = LutExe.get_init_state exe in
  let cpt = 1 in
  let step_str = Printf.sprintf "#step %d\n" cpt in
  if noo then (
    Rif.write oc step_str;
    Rif.flush oc
  );
  let ins = if (boot opt) then Value.OfIdent.empty else
      Rif.read (Verbose.level()>0) stdin (if noo then Some oc else None) in_vars 
  in 
  (* HERE: init input/output pres *)
  let pres = LutExe.get_init_pres exe in
  do_step cpt ctrl ins pres ctrl ins pres
)

(* TEST
let main () = (
  let opt = MainArg.parse Sys.argv in
	let f2p infile mnode =
  		LutExe.make opt  infile mnode
	in
	let _ = f2p ["env.lut"] "main1" in
	let _ = f2p ["env.lut"] "main2" in
	()
)
*)

let main () = (
  let opt = MainArg.parse Sys.argv in
  let oc = 
    match (MainArg.riffile opt) with
      | None -> stdout
      | Some file -> open_out file
  in
    (try 
       let infile = MainArg.infile opt in
       let mnode = MainArg.main_node opt in
       let mnode = if mnode <> "" then mnode else
         let mainprg = Parsers.read_lut infile  in
         let nl = (Syntaxe.pack_node_list mainprg) in
           if nl = [] then "" else List.hd nl
       in
       let _ = 
         if (infile = []) then 
           if MainArg.see_all_options opt then
             (MainArg.full_usage stderr opt; exit 0)
           else
             (
               MainArg.usage stderr opt;
               raise (Global_error "no input file")
             )
       in
         match  MainArg.gen_mode opt with
           | Ocaml -> GenOcamlGlue.f Sys.argv opt
           | Simu ->
               if (MainArg.test_exe opt) 
               then to_exe oc infile mnode opt
               else to_simu oc infile mnode opt
           | Cstubs -> (
               let prog = LutExe.make opt (MainArg.infile opt) mnode in
               let ins  = LutExe.in_var_list prog
               and outs = LutExe.out_var_list prog
               and locs = LutExe.loc_var_list prog
               in
                 Luc2c.option.Luc2c.output <- MainArg.outfile (opt) ;
                 Luc2c.option.Luc2c.main_node <- mnode ;
                 Luc2c.option.Luc2c.seed <- (Some(MainArg.seed (opt)));
                 Luc2c.option.Luc2c.step_mode <- (
                   match (MainArg.step_mode (opt)) with 
                     | Lucky.StepInside -> Luc2c.Inside
                     | Lucky.StepEdges  -> Luc2c.Edges
                     | Lucky.StepVertices -> Luc2c.Vertices)
                 ;
                 Luc2c.option.Luc2c.env <- (MainArg.infile(opt));
                 Luc2c.main ins outs locs
             )
           | GenLuc -> (
               let mainprg = Parsers.read_lut infile  in
               let tlenv = CheckType.check_pack (MainArg.libs (opt)) mainprg in
                 (* type/binding check *)
                 if MainArg.test_lex opt then (
                   List.iter (Parsers.lexemize_lut) infile
                 ) else if (MainArg.test_parse opt) then (
                   (* analyse syntaxique *)
                   SyntaxeDump.dump_pack mainprg; 
                 ) else if (MainArg.test_check opt) then (
                   CheckEnv.dbg_dump tlenv; 
                 ) else (
                   to_luc mainprg tlenv opt
                 )
             )
     with
       | RifIO.Bye 
       | Stop -> (
           (* OK *) 
           Rif.write oc ("\n#end. \n");
           Rif.flush oc;
           )
       | Sys_error(s) -> (
           prerr_string (s^"\n") ; 
           Rif.write oc ("\n#end.\n# a pb occured in Lutin/main.ml: Sys_error("^s^")\n");
           Rif.flush oc;
           exit 1
         )
       | Global_error s -> (
           print_global_error s;
           Rif.write oc ("\n#end.\n# "^s^"\n");
           Rif.flush oc;
           exit 1
         )
       | Parsing.Parse_error -> (
           let str = Printf.sprintf "%s\n"
             (compile_error_string (Lexeme.last_made ()) "syntax error") 
           in
             Printf.eprintf "%s" str;
	          flush stderr;
             Rif.write oc ("\n#end.\n# "^str^"\n");
             Rif.flush oc;
             exit 1
         )
       | LutErrors.Compile_error(lxm,msg) -> (
           let str = Printf.sprintf "%s\n" (compile_error_string lxm msg) in
             Printf.eprintf "%s" str;
	          flush stderr;
             Rif.write oc ("\n#end.\n# "^str^"\n");
             Rif.flush oc;
             exit 1
         )
       | Internal_error (fname,msg) -> (
           print_internal_error fname msg ;
           Rif.write oc ("#\nend.\n# Internal error in "^fname^": "^msg^"\n");
           Rif.flush oc;
           exit 1
         )
       | End_of_file -> (
           Rif.write oc ("\n#end.\n# End of file\n");
           Rif.flush oc
         )
    )
)


let _ = (
  Utils.time_C "main";
  main () ;
  Utils.time_R "main";
  Utils.time_P ();
)
