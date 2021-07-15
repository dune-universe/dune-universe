(* Time-stamp: <modified the 16/03/2020 (at 11:37) by Erwan Jahier> *)
open Soc
open Data
open SocExecValue

(* exported *)
let (do_step : Soc.tbl -> Soc.t -> SocExecValue.ctx -> SocExecValue.ctx) =
  fun soc_tbl soc ctx ->
    let step = match soc.step with [step] -> step | _ ->  assert false in
    let ctx = SocExec.soc_step step soc_tbl soc ctx in
    ctx

(****************************************************************************)
(* XXX duplication of SocExec ! 

Maybe this code below, that is more general, is as (in)efficient ?

*)
let dbg = Lv6Verbose.get_flag "execDbg"
let profile_info = Lv6Verbose.profile_info

let (assign_expr : SocExecValue.ctx -> var_expr -> var_expr -> SocExecValue.ctx) =
  fun ctx ve_in ve_out -> (* ve_out := ve_in (in ctx) *)
  Lv6Verbose.exe
    ~flag:dbg 
    (fun () ->
     print_string ("\nAssigning "^(SocUtils.string_of_filter ve_out) ^
                     " to " ^(SocUtils.string_of_filter ve_in) ^ "... ");
      flush stdout);
    let res =
    { ctx with s =
        let v = SocExecValue.get_value ctx ve_in in
        sadd_partial ctx.s ve_out ctx.cpath v
    }
    in
    Lv6Verbose.exe ~flag:dbg (fun () -> print_string (" Done!"); flush stdout);
    res

let make_simple_sinfo lxm name is os  =
  Some(fun () ->
         let atom =
           {
             RdbgEvent.str  = name; 
             RdbgEvent.file = Lxm.file lxm ; 
             RdbgEvent.line = Lxm.line lxm ,Lxm.line lxm ; 
             RdbgEvent.char = Lxm.cstart lxm, Lxm.cend lxm;
             RdbgEvent.stack =  None;
           }
         in
         {
           RdbgEvent.expr  = Expr.Var (Lxm.str lxm);
           RdbgEvent.atoms = [atom];
           RdbgEvent.more  = None; 
           RdbgEvent.in_subst = is ;
           RdbgEvent.out_subst = os ;
         }
      )
      
let make_sinfo lxm name_opt ectx in_subst out_subst =
  (*   if l <= 0 then None else *)
    Some(fun () ->
         let atom =
           {
             RdbgEvent.str  = (match name_opt with Some n -> n | None -> Lxm.str lxm); 
             (* RdbgEvent.str  = (Lxm.str lxm); *)
             RdbgEvent.file = Lxm.file lxm ; 
             RdbgEvent.line = Lxm.line lxm ,Lxm.line lxm ; 
             RdbgEvent.char = Lxm.cstart lxm, Lxm.cend lxm;
             RdbgEvent.stack = 
               match ectx.RdbgEvent.sinfo with
               | None -> None
               | Some si -> Some (List.hd ((si()).RdbgEvent.atoms));
           }
         in
         {
           RdbgEvent.expr  = Expr.Var (Lxm.str lxm);
           RdbgEvent.atoms = [atom];
           RdbgEvent.more  = None;  (* yet *)
           RdbgEvent.in_subst = in_subst ;
           RdbgEvent.out_subst = out_subst ;
         }
        )
let sinfo_subst arg var =
  match arg with
  | Var v -> v,var
  | Const v -> v,var
  | Field (_,_,t)
  | Index (_,_,t)
  | Slice (_,_,_,_,_,t) ->  (SocUtils.string_of_filter arg, t), var
 
    
let (assign_expr_dbg : Lxm.t -> SocExecValue.ctx -> var_expr -> var_expr -> RdbgEvent.t ->
                    (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) -> RdbgEvent.t) =
  fun lxm ctx ve_in ve_out ectx cont -> (* ve_out := ve_in (in ctx) *)
  let val_ve_in = SocExecValue.get_value ctx ve_in in 
  let res =
    { ctx with
      s =
        let v = val_ve_in in
        sadd_partial ctx.s ve_out ctx.cpath v
    }
  in
  (*   let (datal:Data.subst list) = SocExecValue.get_vals res in  *)
  let (datal:Data.subst list) = ["rhs",val_ve_in] in
  let n = ectx.RdbgEvent.nb in
  let ectx = { ectx with
                RdbgEvent.nb = ectx.RdbgEvent.nb+1;
                RdbgEvent.depth = ectx.RdbgEvent.depth+1;
              }
  in
  let t = data_type_of_var_expr ve_in in
  let sinfo = make_sinfo lxm (Some ":=") ectx
                         [sinfo_subst ve_in  ("rhs", t)]
                         [sinfo_subst ve_out ("lhs", t)]
  in
  let cont2 local_ectx val_ctx =
    let (datal:Data.subst list) =  [("rhs",val_ve_in);("lhs",val_ve_in)] in 
    let nectx = RdbgEvent.incr_event_nb local_ectx in
    let nectx = RdbgEvent.decr_event_depth nectx in
    { ectx with
      RdbgEvent.kind = RdbgEvent.Exit;
      RdbgEvent.name = "Assign";
      RdbgEvent.lang = "lustre";
      RdbgEvent.inputs  = [("rhs", t)];
      RdbgEvent.outputs = ["lhs", t];
      RdbgEvent.locals = [];
      
      RdbgEvent.step = ectx.RdbgEvent.step;
      RdbgEvent.nb = local_ectx.RdbgEvent.nb;
      RdbgEvent.depth = ectx.RdbgEvent.depth;
      RdbgEvent.sinfo = sinfo;
      RdbgEvent.data = datal;
      RdbgEvent.next = (fun () -> cont nectx val_ctx);
      RdbgEvent.terminate = ectx.RdbgEvent.terminate;
      RdbgEvent.reset = ectx.RdbgEvent.reset;
    }
  in
  { ectx with 
    RdbgEvent.kind = RdbgEvent.Call;
    RdbgEvent.name = "Assign";
    RdbgEvent.lang = "lustre";
    RdbgEvent.inputs  = [("rhs", t)];
    RdbgEvent.outputs = ["lhs", t];
    RdbgEvent.locals = [];

    RdbgEvent.sinfo = sinfo;
    RdbgEvent.step = ectx.RdbgEvent.step;
    RdbgEvent.nb = n;
    RdbgEvent.data = datal;
    RdbgEvent.next = (fun () -> cont2 ectx res);
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

let _get_all_subst = SocExecValue.filter_top_subst
let _get_all_subst = SocExecValue.substs_to_data_subst
let get_input_vals val_ctx in_vars =
  let datal = SocExecValue.get_vals val_ctx in
  let datal = List.filter (fun (x,_) -> List.mem_assoc x in_vars||x="_memory") datal in
  datal

(* Generates events that mimick node calls when array or struct are accessed *)
let (gen_access_events :
       Lxm.t -> var_expr -> RdbgEvent.t -> SocExecValue.ctx ->
       (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) -> RdbgEvent.t) =
  fun lxm v ectx val_ctx  cont ->
  let v_str = SocUtils.lustre_string_of_var_expr v in
  let ve,id,short_id,t = match v with
    | Index(ve, i, t) -> ve, v_str,
                         "["^string_of_int i^"]", t
    | Field(ve, id, t) -> ve, "access_field_"^id, "."^id, t
    | _ -> assert false (* sno *)
  in
  let initial_sinfo = ectx.sinfo in
  let top = SocUtils.get_top_var ve in
  let v_in = SocUtils.lustre_string_of_var_expr top, data_type_of_var_expr top in
  let v_out = v_str, t in
  let lxm = Lxm.override_name short_id lxm in
  let sinfo = make_simple_sinfo lxm short_id [v_in,v_in] [v_out,v_out] in
  let val_ctx0 = { val_ctx with cpath = (List.tl val_ctx.cpath) } in
  let val_v_in = SocExecValue.get_value val_ctx0  ve in
  let (data:Data.subst list) = [  fst v_in, val_v_in]  in  
  let cont2 local_ectx val_ctx =
    let nectx = RdbgEvent.incr_event_nb local_ectx in
    let nectx = { nectx with RdbgEvent.sinfo = initial_sinfo } in
    let val_v_out =
      match v, val_v_in with
      | Index(_ve, i, _t) , A a -> a.(i)
      | Field(_ve, id, _t), S fl -> (try List.assoc id fl with _ -> assert false (*sno *))
      | _,U -> U
      | _,_ -> assert false
    in
    let data = (fst v_out, val_v_out)::data in
    { ectx with
      RdbgEvent.step = ectx.RdbgEvent.step;
      RdbgEvent.nb = nectx.RdbgEvent.nb;
      RdbgEvent.depth = ectx.RdbgEvent.depth;
      RdbgEvent.kind = RdbgEvent.Exit;
      RdbgEvent.lang = "lustre";
      RdbgEvent.name = id;
      RdbgEvent.inputs = [v_in];
      RdbgEvent.outputs = [v_out];
      RdbgEvent.locals = [];
      RdbgEvent.sinfo = sinfo;
      RdbgEvent.data = data;
      RdbgEvent.next = (fun () -> cont nectx val_ctx);
      RdbgEvent.terminate = ectx.RdbgEvent.terminate;
      RdbgEvent.reset = ectx.RdbgEvent.reset;
    } 
  in
  { ectx with
    RdbgEvent.step = ectx.RdbgEvent.step;
    RdbgEvent.nb = ectx.RdbgEvent.nb;
    RdbgEvent.depth = ectx.RdbgEvent.depth;
    RdbgEvent.kind = RdbgEvent.Call;
    RdbgEvent.lang = "lustre";
    RdbgEvent.name = id;
    RdbgEvent.inputs = [v_in];
    RdbgEvent.outputs = [v_out];
    RdbgEvent.locals = [];
    RdbgEvent.sinfo = sinfo;
    RdbgEvent.data = data;
    RdbgEvent.next = (fun () -> cont2 ectx val_ctx);
    RdbgEvent.terminate = ectx.RdbgEvent.terminate;
    RdbgEvent.reset = ectx.RdbgEvent.reset;
  } 

let rec (gen_access_events_list :
           Lxm.t -> var_expr list -> RdbgEvent.t -> SocExecValue.ctx ->
           (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) -> RdbgEvent.t) =
  fun lxm vel_in ectx val_ctx cont ->
  let vel_in = List.filter (function Index _ | Field _  -> true | _ -> false) vel_in in
  match vel_in with
  | [] -> cont ectx val_ctx
  | ve_in::tail ->
     let cont ectx val_ctx = gen_access_events lxm ve_in ectx val_ctx cont in
     gen_access_events_list lxm tail  ectx val_ctx cont
    
  
let rec (soc_step :  Lxm.t -> Soc.step_method -> Soc.tbl -> Soc.t ->
                     RdbgEvent.t -> SocExecValue.ctx ->
                     (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) -> RdbgEvent.t) =
  fun _lxm step soc_tbl soc ectx val_ctx cont ->
  profile_info ("SocExecDbg.soc_step \n");
  let soc_name,_,_ = soc.key in
  let lxm = step.lxm in
  let event = 
    match step.impl with
    | Extern ->
       print_string (
           "\nextern nodes and functions not yet supported in the interpreter, sorry.\n"^
             "Please use the C code generator (-2c)."
         );
       exit 2
    | Predef -> (
      try 
        let val_ctx = SocExecEvalPredef.get soc.key val_ctx in
        cont ectx val_ctx 
      with Not_found -> (* Not a predef op *)
        print_string (
            "*** internal error in "^soc_name^". Is it defined in SocExecEvalPredef?\n");
        flush stdout; assert false
    )
    | Gaol(_vl,gaol) -> do_gaol soc_tbl ectx gaol val_ctx cont
                               
    | Boolred(i,j,k) -> (
      (* XXX mettre ce code dans socPredef ? (ou socMetaopPredef)*)
      let inputs, outputs = soc.profile in
      let b_array = (List.hd inputs) in
      let cpt = ref 0 in
      for i = 0 to k-1 do
        let a_i = array_index i b_array in
        let v = SocExecValue.get_value val_ctx a_i in
        if v = B true then incr cpt;
      done;
      let res = B (!cpt >= i && !cpt <= j) in
      let res_var = fst (List.hd outputs) in
      let s = sadd val_ctx.s (res_var::val_ctx.cpath) res in
      cont ectx { val_ctx with s = s }
    )
    | Condact(node_sk, dft_cst) -> (
      let clk = SocExecValue.get_value val_ctx (Var ("activate",Bool)) in
      let vel_in, vel_out = soc.profile in
      let vel_in  =  List.map (fun x -> Var x) (List.tl vel_in) in
      let vel_out =  List.map (fun x -> Var x) vel_out in
      let node_soc = SocUtils.find lxm node_sk soc_tbl in
      let inst_name =
        match soc.instances with
        | [] -> let (proc_name,_,_) = node_soc.key in proc_name
        | [inst] -> fst inst
        | _ -> assert false
      in
      let path_saved = val_ctx.cpath in
      let val_ctx = { val_ctx with cpath=inst_name::val_ctx.cpath } in
      if clk = B true then (
        let node_step = match node_soc.step with
            [step] -> step
          | _ -> assert false
        in
        let cont ectx val_ctx =
          let val_ctx =
            { 
              cpath=path_saved;
              s = sadd val_ctx.s ("_memory"::val_ctx.cpath) (B false)
            }
          in
          cont ectx val_ctx
        in
        do_soc_step lxm None node_step val_ctx soc_tbl node_soc
                    vel_in vel_out ectx cont              
      ) else (
        let first_step = Var ("_memory",Bool) in
        let val_ctx =  { val_ctx with cpath=path_saved } in
        let v = get_value val_ctx first_step in
        let val_ctx =
          if v = U || v = B true then
            (* We are on the first step of node_soc;
                 - we assign the output var to the default values *)
            (assert (List.length dft_cst = List.length vel_out);
             List.fold_left2 assign_expr val_ctx dft_cst vel_out)
              (* XXX use assign_expr_dbg *)
          else
            (* We are not on the first step of node_soc; hence we do nothing 
                 and the output will keep their previous value. *)
            val_ctx
        in
        let val_ctx = { val_ctx with
                        s = sadd val_ctx.s ("_memory"::val_ctx.cpath) (B false) }
        in
        cont ectx val_ctx
      )
    )
    | Iterator(iter, node_sk, n) -> (
      let node_soc = SocUtils.find lxm node_sk soc_tbl in
      let node_step = match node_soc.step with [step] -> step | _ ->  assert false in
      let iter_inputs,iter_outputs = soc.profile in
      let (proc_name,_,_) = node_soc.key in 
      let inst_name =
        match soc.instances with
        | [] -> Array.make n proc_name
        | _  -> Array.of_list (List.map fst soc.instances)
      in
      let path_saved = val_ctx.cpath in
      let add_i i (id,t) = (Printf.sprintf "%s_%d" id i), t in
      let rec f i cont ectx val_ctx = (* iterate over the list of instances *)
        if i < 0 then
          cont ectx val_ctx
        else (
          let vel_in, vel_out =
            match iter with
            | "map" -> (List.map (array_index i) iter_inputs,
                        List.map (array_index i) iter_outputs)
            | "fold" | "red" | "fill"
            | "fillred" ->
               let a_in = (List.hd iter_inputs) in
               let a_in_i, a_in_ip1 =
                 if n = 1 then (* array of size 1 *)
                   Var (a_in), Var (List.hd iter_outputs) 
                 else 
                 if i = 0 then 
                   Var (a_in), Var (add_i 1 a_in) 
                 else 
                   if i = n-1 then
                     Var (add_i i a_in), Var (List.hd iter_outputs) 
                   else
                     Var (add_i i a_in), Var (add_i (i+1) a_in)
               in
               ( a_in_i::(  List.map (array_index i) (List.tl iter_inputs)),
                 a_in_ip1::(List.map (array_index i) (List.tl iter_outputs)))
            | _ -> assert false (* should not occur *)
          in
          let cont ectx val_ctx =
            let val_ctx =  { val_ctx with cpath = path_saved } in
            cont ectx val_ctx
          in
          let cont ectx val_ctx =
            let lxm = node_step.lxm in
            let val_ctx = { val_ctx with cpath = inst_name.(i)::val_ctx.cpath } in
            do_soc_step lxm (Some i) node_step val_ctx
                        soc_tbl node_soc vel_in vel_out ectx cont
          in
          f (i-1) cont ectx val_ctx
        )
               (* XXX use assign_expr_dbg to let rdbg step into iterators *)
      in
      f (n-1) cont ectx val_ctx
    )
  in
  event

and (do_gaol : Soc.tbl -> RdbgEvent.t -> gao list -> SocExecValue.ctx ->
               (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) ->  RdbgEvent.t) =
  fun soc_tbl ectx gaol val_ctx cont ->
  match gaol with
  | [] -> assert false
  | [gao] -> do_gao soc_tbl ectx gao val_ctx cont
  | gao::gaol ->
     let cont ectx val_ctx = do_gaol soc_tbl ectx gaol val_ctx cont in
     do_gao soc_tbl ectx gao val_ctx cont

and (do_gao : Soc.tbl -> RdbgEvent.t -> gao -> SocExecValue.ctx ->
              (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) ->  RdbgEvent.t)  =
  fun soc_tbl ectx gao val_ctx cont -> 
  match gao with
  | Case(id, id_gao_l,lxm) -> (
    try 
      let id_val = get_enum id val_ctx in
      let gaol = List.assoc id_val id_gao_l in
      (*       do_gaol soc_tbl ectx gaol val_ctx cont *)
      
      let v = Data.E(id_val, SocUtils.get_rank id_val id_gao_l) in 
      let t = Enum(id,List.map fst id_gao_l) in
      let clk_var = Var(id,t) in
      let (datal:Data.subst list) = ["clock", v] in
      
      let clock = ("clock", t) in
      let n = ectx.RdbgEvent.nb in
      let ectx = { ectx with
                   RdbgEvent.nb = ectx.RdbgEvent.nb+1;
                   (*   RdbgEvent.depth = ectx.RdbgEvent.depth+1; *)
                 }
      in
      let sinfo = make_sinfo lxm (Some "when") ectx
                             [sinfo_subst clk_var clock]
                             []
      in
      let cont2 local_ectx val_ctx =
        let nectx = RdbgEvent.incr_event_nb local_ectx in
        (*         let nectx = RdbgEvent.decr_event_depth nectx in *)
        { ectx with
          RdbgEvent.kind = RdbgEvent.Exit;
          RdbgEvent.name = "when";
          RdbgEvent.lang = "lustre";
          RdbgEvent.inputs  = [clock];
          RdbgEvent.outputs = [];
          RdbgEvent.locals = [];
          
          RdbgEvent.step = ectx.RdbgEvent.step;
          RdbgEvent.nb = local_ectx.RdbgEvent.nb;
          RdbgEvent.depth = ectx.RdbgEvent.depth+1;
          RdbgEvent.sinfo = sinfo;
          RdbgEvent.data = datal;
          RdbgEvent.next = (fun () -> cont nectx val_ctx);
          RdbgEvent.terminate = ectx.RdbgEvent.terminate;
          RdbgEvent.reset = ectx.RdbgEvent.reset;
        } 
      in
      { ectx with 
        RdbgEvent.kind = RdbgEvent.Call;
        RdbgEvent.name = "when";
        RdbgEvent.lang = "lustre";
        RdbgEvent.inputs  = [clock];
        RdbgEvent.outputs = [];
        RdbgEvent.locals = [];

        RdbgEvent.depth = ectx.RdbgEvent.depth+1;
        RdbgEvent.sinfo = sinfo;
        RdbgEvent.step = ectx.RdbgEvent.step;
        RdbgEvent.nb = n;
        RdbgEvent.data = datal;
        RdbgEvent.next = (fun () -> do_gaol soc_tbl ectx gaol val_ctx cont2);
      } 

        
    with Not_found -> cont ectx val_ctx 
  )
  | Call(vel_out, Assign, vel_in, lxm) -> (
    assert (List.length vel_in = List.length vel_out);
    (*
    let val_ctx = 
      assert (List.length vel_in = List.length vel_out);
      List.fold_left2 assign_expr val_ctx vel_in vel_out
    in
    cont ectx val_ctx
     *)
    let fcont = 
      List.fold_left2
        (fun acc_cont ve_in ve_out ->
         let ncont ectx val_ctx =
           assign_expr_dbg lxm val_ctx ve_in ve_out ectx acc_cont in
         ncont
        )
        cont
        vel_in
        vel_out 
    in
    fcont ectx val_ctx 
  )
  | Call(vel_out, Procedure sk, vel_in, lxm) -> (
    let (proc_name,_,_) = sk in
    let path_saved = val_ctx.cpath in
    let val_ctx = { val_ctx with cpath = proc_name::val_ctx.cpath } in
    let soc = SocUtils.find lxm sk soc_tbl in
    let step = match soc.step with [step] -> step | _ ->  assert false in
    let cont ectx val_ctx =
      let val_ctx =  { val_ctx with cpath = path_saved } in
      cont ectx val_ctx
    in
    let cont ectx val_ctx =
      do_soc_step lxm None step val_ctx soc_tbl soc vel_in vel_out ectx cont
    in
    cont ectx val_ctx 
  )
  | Call(vel_out, Method((inst_name,sk),step_name), vel_in, lxm) -> (
    let path_saved = val_ctx.cpath in
    let val_ctx = { val_ctx with cpath = inst_name::val_ctx.cpath } in
    let soc = SocUtils.find lxm sk soc_tbl in
    let step = try List.find (fun sm -> sm.name = step_name) soc.step
               with Not_found -> assert false
    in
    let cont ectx val_ctx =
      let val_ctx = { val_ctx with cpath = path_saved } in
      cont ectx val_ctx
    in
    let cont ectx val_ctx = 
      do_soc_step lxm None step val_ctx soc_tbl soc vel_in vel_out ectx cont
    in
    cont ectx val_ctx 
  )
and (do_soc_step : Lxm.t -> int option -> step_method -> SocExecValue.ctx ->
                   Soc.tbl -> Soc.t ->  var_expr list -> var_expr list -> RdbgEvent.t -> 
                   (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) ->  RdbgEvent.t) =
  fun lxm int_opt step val_ctx0 soc_tbl soc vel_in vel_out ectx0 cont0 -> 
  let (soc_name,_,_) = soc.key in
  profile_info ("SocExecDbg.do_soc_step "^soc_name^"\n");
  let soc_in_vars, soc_out_vars = soc.profile in
  let step_in_vars = filter_params soc soc_in_vars step.idx_ins in 
  let step_out_vars = filter_params soc soc_out_vars step.idx_outs in
  let new_s = substitute_args_and_params vel_in step_in_vars val_ctx0 in
  let step_name = match soc.step with
      [_] -> soc_name
    | _ -> soc_name^"."^step.name
  in
  let step_name = match int_opt with
    | Some i -> Printf.sprintf "%s_%d" step_name i
    | None -> step_name
  in
  let locals = if soc.step = [] then [] else
                 match (List.hd soc.step).impl with
                 | Gaol (var_list, _) -> var_list
                 | Iterator _
                 | Boolred _
                 | Condact _
                 | Extern 
                 | Predef  -> []
  in
  let locals = match soc.memory with
    | No_mem | Mem_hidden  -> locals
    | Mem dt -> ("_memory",dt)::locals
  in
  let name_opt =
    (* the long names of lustre op are boring *)
    if String.length step_name > 8 && String.sub step_name 0 8 = "Lustre::" then
      None else Some step_name
  in
  let initial_sinfo = ectx0.RdbgEvent.sinfo in
  let ectx0 = {
    ectx0 with
    RdbgEvent.depth = ectx0.RdbgEvent.depth+1;
  }
  in
  let sinfo = make_sinfo lxm name_opt  ectx0
                         (List.map2 sinfo_subst vel_in step_in_vars)
                         (List.map2 sinfo_subst vel_out step_out_vars)
  in
  let cont2 local_ectx val_ctx =
    let cont3 ectx val_ctx =
      let s_out = substitute_params_and_args step_out_vars vel_out val_ctx in
      cont0 ectx { val_ctx with s = s_out }
    in
    let (datal:Data.subst list) = SocExecValue.get_vals val_ctx in 
    let nectx = RdbgEvent.incr_event_nb local_ectx in
    let nectx = RdbgEvent.decr_event_depth nectx in
    let nectx = { nectx with RdbgEvent.sinfo = initial_sinfo } in
    { nectx with
      RdbgEvent.step = nectx.RdbgEvent.step;
      RdbgEvent.nb = local_ectx.RdbgEvent.nb;
      RdbgEvent.depth = ectx0.RdbgEvent.depth;
      RdbgEvent.kind = RdbgEvent.Exit;
      RdbgEvent.lang = "lustre";
      RdbgEvent.name = step_name;
      RdbgEvent.inputs = fst soc.profile;
      RdbgEvent.outputs = snd soc.profile;
      RdbgEvent.locals = locals;
      RdbgEvent.sinfo = sinfo;
      RdbgEvent.data = datal;
      RdbgEvent.next = (fun () -> cont3 nectx val_ctx);
      RdbgEvent.terminate = nectx.RdbgEvent.terminate;
      RdbgEvent.reset = nectx.RdbgEvent.reset;
    } 
  in
  let cont1 lectx val_ctx =
    let ectx = {
      lectx with
      RdbgEvent.name = step_name;
      RdbgEvent.inputs  = fst soc.profile;
      RdbgEvent.outputs = snd soc.profile;
      RdbgEvent.locals = locals;
      RdbgEvent.sinfo = sinfo;
    }
    in
    let val_ctx =  { val_ctx with s = new_s } in
    (*     let (datal:Data.subst list) = get_all_subst val_ctx.s in *)
    let datal = get_input_vals val_ctx step_in_vars in   
    {  ectx with 
      RdbgEvent.kind = RdbgEvent.Call;
      RdbgEvent.lang = "lustre";
      RdbgEvent.data = datal;
      RdbgEvent.next = (fun () ->
                    assert (List.mem  step soc.step);
                    let ectx = RdbgEvent.incr_event_nb ectx in
                    soc_step lxm step soc_tbl soc ectx val_ctx cont2
                   );
    }
  in
  gen_access_events_list step.lxm vel_in ectx0 val_ctx0 cont1


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

(* End of XXX duplication of SocExec !  *)
(****************************************************************************)
    
(* exported *) 
and (do_step_dbg : Soc.tbl -> Soc.t -> RdbgEvent.t -> SocExecValue.ctx ->
                   (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) -> RdbgEvent.t) =
  fun soc_tbl soc ectx val_ctx cont ->
  let soc_in_vars, _soc_out_vars = soc.profile in
  (* let (datal:Data.subst list) = get_all_subst val_ctx.s in  *)
  let (datal:Data.subst list) = get_input_vals val_ctx soc_in_vars in 
  let (soc_name,_,_) = soc.key in
  let lxm = (List.hd soc.step).lxm in
  let locals = if soc.step = [] then [] else
                 match (List.hd soc.step).impl with
                 | Gaol (var_list, _) -> var_list
                 | Iterator _
                 | Boolred _
                 | Condact _
                 | Predef 
                 | Extern -> []
  in
  let sinfo = 
    Some(fun () ->
         let atom =
           {
             RdbgEvent.str  = soc_name;
             RdbgEvent.file = Lxm.file lxm; 
             RdbgEvent.line = Lxm.line lxm,Lxm.line lxm; 
             RdbgEvent.char = Lxm.cstart lxm, Lxm.cend lxm;
             RdbgEvent.stack = None;
           }
         in
         {
           RdbgEvent.expr  = Expr.Var "dummy" ; (* XXX *)
           RdbgEvent.atoms = [atom];
           RdbgEvent.more  = None;  (* yet *)
           RdbgEvent.in_subst = [];
           RdbgEvent.out_subst = [];
         }
        )
  in
  let ectx = { 
    ectx with 
    RdbgEvent.name = soc_name;
    RdbgEvent.depth = ectx.RdbgEvent.depth+1;
    RdbgEvent.data = datal;
    RdbgEvent.inputs  = fst soc.profile;
    RdbgEvent.outputs = snd soc.profile;
    RdbgEvent.locals = locals;
    RdbgEvent.sinfo = sinfo;
  } 
  in
  let cont2 local_ectx val_ctx =
    (* je pourrais enlver les entrées... *)
    let (datal:Data.subst list) = SocExecValue.get_vals val_ctx in 
    let nectx = { ectx with
                  RdbgEvent.nb = local_ectx.RdbgEvent.nb+1;
                  RdbgEvent.data = datal;
                  RdbgEvent.depth = ectx.RdbgEvent.depth-1;
                }
    in
    { ectx with
      RdbgEvent.step = ectx.RdbgEvent.step;
      RdbgEvent.nb = local_ectx.RdbgEvent.nb;
      RdbgEvent.depth = ectx.RdbgEvent.depth;
      RdbgEvent.kind = RdbgEvent.Exit;
      RdbgEvent.lang = "lustre";
      RdbgEvent.name = soc_name;
      RdbgEvent.inputs  = fst soc.profile;
      RdbgEvent.outputs = snd soc.profile;
      RdbgEvent.locals = locals;
      RdbgEvent.sinfo = sinfo;
      RdbgEvent.data = datal;
      RdbgEvent.next = (fun () -> cont nectx val_ctx);
      RdbgEvent.terminate = ectx.RdbgEvent.terminate;
      RdbgEvent.reset = ectx.RdbgEvent.reset;
    } 
  in
  { ectx with
    RdbgEvent.step = ectx.RdbgEvent.step;
    RdbgEvent.nb = ectx.RdbgEvent.nb;
    RdbgEvent.depth = ectx.RdbgEvent.depth;
    RdbgEvent.kind = RdbgEvent.Call;
    RdbgEvent.lang = "lustre";
    RdbgEvent.name = soc_name;
    RdbgEvent.inputs = ectx.RdbgEvent.inputs;
    RdbgEvent.outputs = ectx.RdbgEvent.outputs;
    RdbgEvent.locals = locals;
    RdbgEvent.sinfo = sinfo;
    RdbgEvent.data = ectx.RdbgEvent.data;
    RdbgEvent.next = (fun () -> 
                  let step = match soc.step with [step] -> step | _ ->  assert false in
                  let ectx = RdbgEvent.incr_event_nb ectx in
                  soc_step step.lxm step soc_tbl soc ectx val_ctx cont2
                 );
    RdbgEvent.terminate = ectx.RdbgEvent.terminate;
    RdbgEvent.reset = ectx.RdbgEvent.reset;
  }
