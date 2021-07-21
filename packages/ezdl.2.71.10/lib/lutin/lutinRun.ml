(* Time-stamp: <modified the 16/03/2020 (at 11:38) by Erwan Jahier> *)
(**********************************************************************************)

let (var_to_var_pair: Exp.var -> string * Data.t) =
  fun v -> Var.name v, Type.to_data_t (Var.typ v)

let (to_subst_list : (string * Data.t) list -> Value.OfIdent.t -> Data.subst list) =
  fun var_decl vals -> 
    try List.map (fun (n,_) -> n, Value.to_data_val (Value.OfIdent.get vals n)) var_decl
    with Not_found -> assert false

(* ditto, but without taking care of variable order *)
let (_from_vals : Value.OfIdent.t -> Data.subst list) = fun vals -> 
  Value.OfIdent.fold (fun id v acc -> (id,Value.to_data_val v)::acc) vals []

let (to_vals : Data.subst list -> Value.OfIdent.t) =
  List.fold_left
    (fun acc (n,v) -> Value.OfIdent.add acc (n, Value.from_data_val v))
    Value.OfIdent.empty

open RdbgPlugin
type ctx = RdbgEvent.t

let compact str =
  let str = Str.global_replace (Str.regexp "\n") ";" str in
  let str = Str.global_replace (Str.regexp "[ \t]+") "" str in
  str

let make argv =
  let opt = MainArg.parse argv in
  let prog = MainArg.infile opt
  and node = MainArg.main_node opt
  in
  let seed = MainArg.seed opt in
  (*   MainArg.set_seed opt (Some seed); *)
  let lut_mach = LutExe.make opt prog node in
  let lut_in  = List.map var_to_var_pair (LutExe.in_var_list  lut_mach) in 
  let lut_out = List.map var_to_var_pair (LutExe.out_var_list lut_mach) in 
  let lut_memories =
    (*     if LtopArg.args.LtopArg.delay_env_outputs then *)
    (*       LutExe.get_init_pres lut_mach *)
    (*     else *)
    Value.OfIdent.empty
  in
  let tables = ref lut_mach in 
  let ctrl_state = ref (LutExe.get_init_state lut_mach) in
  let data_state = ref
      { LutExe.ins = Value.OfIdent.empty;
        LutExe.outs = lut_memories;
        LutExe.mems = LutExe.get_init_pres lut_mach
      }
  in
  let ss_table = Hashtbl.create 10 in

  let lut_step sl =
    let _ = data_state := { !data_state with LutExe.ins = to_vals sl } in
    let new_tables, new_cs, new_ds = LutExe.step !tables !ctrl_state !data_state in
    tables := new_tables;
    ctrl_state := new_cs;
    data_state := new_ds;
    to_subst_list lut_out new_ds.LutExe.outs
  in
  let (lut_step_dbg: 
         Data.subst list -> ctx ->       
       (Data.subst list -> ctx -> RdbgEvent.t) -> RdbgEvent.t) =
    fun sl ctx cont -> 
      let cont_lut_step ctx = 
        fun new_tables new_cs new_ds -> 
          tables := new_tables;
          ctrl_state := new_cs;
          data_state := new_ds;
          cont (to_subst_list lut_out new_ds.LutExe.outs) ctx
      in
      data_state := { !data_state with LutExe.ins = to_vals sl };
      LutExe.step_rdbg ctx node !tables !ctrl_state !data_state cont_lut_step
  in
  let mems_in = 
    List.fold_left
      (fun acc (vn,_vt) -> 
         try 
           let v = Value.OfIdent.get lut_memories vn in
           (vn, Value.to_data_val v)::acc
         with Not_found -> acc
      )
      []
      lut_in
  in
  let mems_out = 
    List.fold_left
      (fun acc (vn,_vt) -> 
         try 
           let v = Value.OfIdent.get lut_memories vn in
           (vn, Value.to_data_val v)::acc
         with Not_found -> acc
      )
      []
      lut_out
  in
  let argv_list = Array.to_list argv in 
  let argv_str = String.concat " " argv_list in
  let id =
    if List.mem "-seed" argv_list then argv_str else
      argv_str ^ " -seed " ^ (string_of_int seed)
  in
  let version = Printf.sprintf "with lutin Version %s (\"%s\")"
      Version.str Version.sha in
  {
    id = Printf.sprintf "%s (%s)" id version;
    inputs = lut_in;
    outputs= lut_out;
    reset = (fun () -> (
          tables := lut_mach;
          ctrl_state := (LutExe.get_init_state lut_mach);
          data_state := 
            { LutExe.ins = Value.OfIdent.empty;
              LutExe.outs = lut_memories;
              LutExe.mems = LutExe.get_init_pres lut_mach
            }
        ));
    kill=(fun _ -> ());
    save_state = (fun i ->
        let prgs = Random.get_state () in
        if Verbose.level() > 0 then (
          Printf.eprintf "Save state %i from Lutin %s (%i)\n" i node
            (Random.State.bits (Random.State.copy prgs));
          flush stderr);
        Hashtbl.replace ss_table (i,node)
          (!tables, !ctrl_state, !data_state, prgs)
      );
    restore_state = (fun i ->
        match Hashtbl.find_opt ss_table (i,node) with
        | Some (tbl,cs, ds, prgs) ->
          if Verbose.level() > 0 then (          
            Printf.eprintf
              "Restore state %i from %s:%s\n\tPRGS:%i\n\tins:%s\n\touts:%s\n\tmems:%s\n"
              i (String.concat "+" prog) node
              (Random.State.bits (Random.State.copy prgs))
              (compact (Value.OfIdent.to_string "" ds.LutExe.ins))
              (compact (Value.OfIdent.to_string "" ds.LutExe.outs))
              (compact (Value.OfIdent.to_string "" ds.LutExe.mems))

            ;
            flush stderr
          );
          tables := tbl;
          ctrl_state := cs; data_state := ds;
          Random.set_state prgs;
        | None  ->
          Printf.eprintf "Cannot restore state %i from Lutin\n" i; flush stderr 
      );
    init_inputs=mems_in;
    init_outputs=mems_out;
    step=lut_step;     
    step_dbg=lut_step_dbg;
  }




