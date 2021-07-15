(* Time-stamp: <modified the 18/09/2019 (at 11:19) by Erwan Jahier> *)

open Soc
open Data
open SocExecValue

let dbg = Lv6Verbose.get_flag "exec"
let profile_info = Lv6Verbose.profile_info

let (assign_expr : ctx -> var_expr -> var_expr -> ctx) =
  fun ctx ve_in ve_out -> (* ve_out := ve_in (in ctx) *)
    Lv6Verbose.exe ~flag:dbg 
      (fun () ->
        print_string ("\nAssigning "^(SocUtils.string_of_filter ve_out) ^
                         " to " ^(SocUtils.string_of_filter ve_in) ^ "\n");
        flush stdout);
    { ctx with s =
        let v = SocExecValue.get_value ctx ve_in in
        sadd_partial ctx.s ve_out ctx.cpath v 
    }

(* [array_index i v] returns the var_expr v[i] *)
let (array_index : int -> var -> var_expr) =
  fun i (vn,vt) -> 
    let vt_elt = 
      match vt with
        | Array(vt_elt,_) -> vt_elt
        | _ -> assert false
    in
    Index(Var(vn,vt),i,vt_elt)

let rec (soc_step : Soc.step_method -> Soc.tbl -> Soc.t -> SocExecValue.ctx
                    -> SocExecValue.ctx) =
  fun step soc_tbl soc ctx ->
  profile_info ("SocExec.soc_step \n");
  let soc_name,_,_ = soc.key in
  let ctx = 
    match step.impl with
    | Extern ->
       print_string (
           "\nextern nodes and functions not yet supported in the interpreter, sorry.\n"^
             "Please use the C code generator (-2c)."
         );
       exit 2
    | Predef -> (
      try 
        let ctx = SocExecEvalPredef.get soc.key ctx in
        ctx
      with Not_found -> (* Not a predef op *) print_string (
          "*** internal error in "^soc_name^". Is it defined in SocExecEvalPredef?\n");
        flush stdout; assert false
    )
    | Gaol(_vl,gaol) -> List.fold_left (do_gao step.lxm soc_tbl) ctx gaol
    | Boolred(i,j,k) -> (
      (* XXX mettre ce code dans socPredef ? (ou socMetaopPredef)*)
      let inputs, outputs = soc.profile in
      let b_array = (List.hd inputs) in
      let cpt = ref 0 in
      for i = 0 to k-1 do
        let a_i = array_index i b_array in
        let v = SocExecValue.get_value ctx a_i in
        if v = B true then incr cpt;
      done;
      let res = B (!cpt >= i && !cpt <= j) in
      let res_var = fst (List.hd outputs) in
      let s = sadd ctx.s (res_var::ctx.cpath) res in
      { ctx with s = s }
    )
    | Condact(node_sk, dft_cst) -> (
      let clk = SocExecValue.get_value ctx (Var ("activate",Bool)) in
      let vel_in, vel_out = soc.profile in
      assert(vel_in <> []);
      let vel_in  =  List.map (fun x -> Var x) (List.tl vel_in) in
      let vel_out =  List.map (fun x -> Var x) vel_out in
      let node_soc = SocUtils.find step.lxm node_sk soc_tbl in
      let inst_name =
        match soc.instances with
        | [] -> let (proc_name,_,_) = node_soc.key in proc_name
        | [inst] -> fst inst
        | _ -> assert false
      in
      let path_saved = ctx.cpath in
      let ctx = { ctx with cpath=inst_name::ctx.cpath } in
      let ctx =
        if clk = B true then
          let node_step = match node_soc.step with
              [step] -> step
            | _ -> assert false
          in
          let ctx = do_step inst_name node_step ctx soc_tbl node_soc vel_in vel_out in
          { ctx with cpath=path_saved }
        else
          let first_step = Var ("_memory",Bool) in
          let ctx =  { ctx with cpath=path_saved } in
          let v = get_value ctx first_step in
          if v = U || v = B true then
            (* We are on the first step of node_soc;
                   - we assign the output var to the default values *)
            (assert (List.length dft_cst = List.length vel_out);
             List.fold_left2 assign_expr ctx dft_cst vel_out)
          else
            (* We are not on the first step of node_soc; hence we do nothing 
                   and the output will keep their previous value. *) 
            ctx
      in
      let ctx = { ctx with s = sadd ctx.s ("_memory"::ctx.cpath) (B false) } in
      ctx
    )
    | Iterator(iter, node_sk, n) -> 
       let node_soc = SocUtils.find step.lxm node_sk soc_tbl in
       let node_step = match node_soc.step with [step] -> step | _ ->  assert false in
       let iter_inputs,iter_outputs = soc.profile in
       let (proc_name,_,_) = node_soc.key in 
       let inst_name =
         match soc.instances with
         | [] -> Array.make n proc_name
         | _  -> Array.of_list (List.map fst soc.instances)
       in
       let rec f i ctx = (* iterate over the list of instances *)
         if i=n then ctx else
           let vel_in, vel_out =
             match iter with
             | "map" -> (List.map (array_index i) iter_inputs,
                         List.map (array_index i) iter_outputs)
             | "fold" | "red" | "fill"
             | "fillred" ->
               let a_in = Var (List.hd iter_inputs) in
                (
                  assert(iter_inputs <> []);
                  assert(iter_outputs <> []);
                  a_in::(List.map (array_index i) (List.tl iter_inputs)),
                  a_in::(List.map (array_index i) (List.tl iter_outputs)))
             | _ -> assert false (* should not occur *)
           in
           let ctx = { ctx with cpath = inst_name.(i)::ctx.cpath } in
           let ctx =
             do_step inst_name.(i) node_step ctx soc_tbl node_soc vel_in vel_out
           in
           let ctx = { ctx with cpath = List.tl ctx.cpath } in
           f (i+1) ctx
       in
       let ctx = f 0 ctx in
       let ctx = 
         if iter <> "map" then
           let a_in  = Var (List.hd iter_inputs) in
           let a_out = Var (List.hd iter_outputs) in
           assign_expr ctx a_in a_out  (* a_out=a_n *)
         else
           ctx
       in
       ctx
  in
  ctx

and (do_gao : Lxm.t -> Soc.tbl -> SocExecValue.ctx -> gao -> SocExecValue.ctx) =
  fun _lxm soc_tbl ctx gao ->
  match gao with
  | Case(id, id_gao_l,lxm) -> (
    try 
      let id_val = get_enum id ctx in
      let gaol = List.assoc id_val id_gao_l in
      let ctx = List.fold_left (do_gao lxm soc_tbl) ctx gaol in
      ctx
    with Not_found -> ctx
  )
  | Call(vel_out, Assign, vel_in,_lxm) -> (
    let ctx = 
      assert (List.length vel_in = List.length vel_out);
      List.fold_left2 assign_expr ctx vel_in vel_out
    in
    ctx
  )
  | Call(vel_out, Procedure sk, vel_in,lxm) -> (
    let (proc_name,_,_) = sk in
    let path_saved = ctx.cpath in
    let ctx = { ctx with cpath = proc_name::ctx.cpath } in
    let soc = SocUtils.find lxm sk soc_tbl in
    let step = match soc.step with [step] -> step | _ ->  assert false in
    let ctx = do_step proc_name step ctx soc_tbl soc vel_in vel_out in
    { ctx with 
      cpath = path_saved 
    } 
  )
  | Call(vel_out, Method((inst_name,sk),step_name), vel_in,lxm) -> (
    let path_saved = ctx.cpath in
    let ctx = { ctx with cpath = inst_name::ctx.cpath } in
    let soc = SocUtils.find lxm sk soc_tbl in
    let step = try List.find (fun sm -> sm.name = step_name) soc.step
               with Not_found -> assert false
    in
    let ctx = do_step inst_name step ctx soc_tbl soc vel_in vel_out in
    let ctx = { ctx with cpath = path_saved } in
    ctx
  )
and (do_step : Lv6Id.t -> step_method -> SocExecValue.ctx -> Soc.tbl -> Soc.t -> 
               var_expr list -> var_expr list -> SocExecValue.ctx) =
  fun name step ctx soc_tbl soc vel_in vel_out -> 
  profile_info ("SocExec.do_step "^name^"\n");
  let soc_in_vars, soc_out_vars = soc.profile in
  let step_in_vars = filter_params soc soc_in_vars step.idx_ins in 
  let step_out_vars = filter_params soc soc_out_vars step.idx_outs in
  let new_s = substitute_args_and_params vel_in step_in_vars ctx in
  let ctx = soc_step step soc_tbl soc { ctx with s=new_s } in
  let s_out = substitute_params_and_args step_out_vars vel_out ctx in
  { ctx with s = s_out }

(* get the step params from its soc params *)
and (filter_params : Soc.t -> Soc.var list -> int list -> Soc.var list) =
  fun _soc el il ->
  let local_nth i l = 
    try List.nth l i 
    with _ -> 
      print_string (
          "\n*** Cannot get the " ^ (string_of_int (i+1)) ^ 
            "the element of a list of size " ^ (string_of_int (List.length l))^"\n");
      flush stdout;
      assert false 
  in
  let res  = List.map (fun i -> local_nth i el) il  in
  res


(* [add_data_subst vtnl data_s s] add the data_s to s; *)
let (add_data_subst : var list -> Data.subst list -> SocExecValue.substs
         -> SocExecValue.substs) =
  fun vntl_i s ctx_s -> 
    let s = SocVar.unexpand_profile s vntl_i in
    List.fold_left (fun acc (id,v) -> sadd acc [id] v) ctx_s s

let (read_soc_input : Lv6MainArgs.t -> var list -> Data.vntl -> out_channel ->
     substs -> substs) =
  fun opt vntl_i exp_vntl_i_str oc ctx_s -> 
      let s:Data.subst list =
        RifIO.read ~debug:(Lv6Verbose.level()>0) stdin 
          (if opt.Lv6MainArgs.rif then None else Some oc) exp_vntl_i_str
      in
      add_data_subst vntl_i s ctx_s

exception AssertViolation of Lxm.t
let (check_assertions : SocExecValue.ctx -> Lxm.t * Soc.var -> unit) =
  fun ctx (lxm,(v,t)) ->
  assert(t=Data.Bool);
  match SocExecValue.get_val v ctx with
  | Data.B true -> ()
  | Data.B false -> raise (AssertViolation lxm)
  | _ -> assert false (* sno *)

let rec (loop_step: Lv6MainArgs.t -> Soc.tbl -> Soc.var list -> Data.vntl -> Data.vntl ->
         SocExecValue.ctx -> Data.vntl -> Data.vntl -> Soc.t -> SocExecValue.ctx -> int ->
         out_channel -> unit) =
  fun opt soc_tbl vntl_i init_exp_vntl_i_str init_exp_vntl_o_str init_ctx
    exp_vntl_i_str exp_vntl_o_str soc ctx step_nb oc ->
    try
    if not opt.Lv6MainArgs.rif then
      RifIO.write oc ("\n#step " ^ (string_of_int step_nb)^"\n");
    let ctx = { ctx with s = read_soc_input opt vntl_i exp_vntl_i_str oc ctx.s } in
    let step = match soc.step with [step] -> step | _ ->  assert false in
    let ctx = soc_step step soc_tbl soc ctx in
    let s = SocExecValue.filter_top_subst ctx.s in
    let s = List.flatten(List.map SocVar.expand_subst s) in
    let f2s = SocUtils.my_string_of_float_precision opt.Lv6MainArgs.precision in
    let step_nb = step_nb + 1 in
    if not opt.Lv6MainArgs.rif then RifIO.write oc " #outs ";
    RifIO.write_outputs oc f2s exp_vntl_o_str s;
    RifIO.write oc "\n";
    RifIO.flush oc;
    Lv6Verbose.exe ~flag:dbg (fun () -> dump_substs ctx.s; flush stdout);
    List.iter (check_assertions ctx) soc.assertions;
      loop_step opt soc_tbl vntl_i init_exp_vntl_i_str init_exp_vntl_o_str init_ctx
         exp_vntl_i_str exp_vntl_o_str soc ctx step_nb oc
    with RifIO.Reset ->
      let n,_,_ = soc.key in
      Printf.eprintf "\nW: Reseting lustre node %s.\n" n;
      flush stderr;
      loop_step opt soc_tbl vntl_i init_exp_vntl_i_str init_exp_vntl_o_str init_ctx
        init_exp_vntl_i_str init_exp_vntl_o_str soc init_ctx step_nb oc

let (f : Lv6MainArgs.t -> Soc.tbl -> Soc.key -> unit) =
  fun opt soc_tbl sk ->
    let soc = try SocMap.find sk soc_tbl with Not_found -> assert false in
    let ctx = SocExecValue.create_ctx soc_tbl soc in
    let exp_vntl_i = SocVar.expand_profile true false (fst soc.profile) in
    let exp_vntl_o = SocVar.expand_profile true false (snd soc.profile) in
    let oc = 
      if  opt.Lv6MainArgs.outfile  = "" then stdout else
        let rif_file =
          try (Filename.chop_extension opt.Lv6MainArgs.outfile) ^ ".rif" 
          with _  -> opt.Lv6MainArgs.outfile ^ ".rif"
        in
        open_out rif_file 
    in
    Lv6util.entete oc "#" "";
    RifIO.write_interface oc exp_vntl_i exp_vntl_o None None;
    RifIO.flush oc;
    try
      loop_step opt soc_tbl (fst soc.profile)
        exp_vntl_i exp_vntl_o ctx exp_vntl_i exp_vntl_o soc ctx 1 oc
    with RifIO.Bye -> close_out oc
                                            
(**************************************************************************************)

