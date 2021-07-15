(* Time-stamp: <modified the 22/03/2021 (at 16:37) by Erwan Jahier> *)


open Lxm
open Lic
    
let dbg = (Lv6Verbose.get_flag "en")

(* pack useful info into a single struct *)
type local_ctx = { 
  init_prg : LicPrg.t; (* nodes before expansion *)
  curr_prg : LicPrg.t; (* nodes that have already been expanded *)
  keep_node_call : Lic.node_key -> bool; (* node calls to expand *)
}

(********************************************************************************)
let new_var label str t c =
  let vi = FreshName.var_info str t c in
  Lv6Verbose.exe
    ~flag:dbg 
    (fun () -> Printf.printf "#EN: new_var[%s](%s)->%s%s'\n"
                             label str vi.var_name_eff (Lic.string_of_clock (snd c)) 
    );
  vi

(********************************************************************************)
let get_locals node =
  match node.loclist_eff with
    | None -> []
    | Some l -> l

let (get_node_body : local_ctx -> Lic.node_exp -> Lic.node_body option) =
  fun _lctx node ->
    match node.def_eff with
      | ExternLic
      | AbstractLic None -> None
      | AbstractLic (Some _node) -> assert false
      | BodyLic node_body -> Some node_body
      | MetaOpLic -> None

(********************************************************************************)
(* When expanding a node call such as 

    x = toto(a when c);

   when toto params are (for example) on the base clock, we need to
   propagate the substitution between the toto base clock and "on(c)"
   ; for the toto arguments, it's been done during clock checking ;
   but we need to remember this information to substitute the clock
   of toto in its local variables.
*)
type new_base = clock

(* to hold the substitution of parameters by arguments *)
module IdMap = Map.Make(struct type t = Lv6Id.t let compare = compare end)
type subst = Lic.var_info IdMap.t 

let (add_subst : subst -> Lv6Id.t -> Lic.var_info -> subst) =
  fun s id vi -> IdMap.add id vi s

let (subst_in_id : subst -> Lv6Id.t -> Lic.var_info) =
  fun s id -> IdMap.find id s

let (subst_in_id_id : subst -> Lv6Id.t -> Lv6Id.t) =
  fun s id -> 
  try (subst_in_id s id).var_name_eff with Not_found -> id
                      
let rec (substitute : subst * new_base ->
                      Lic.eq_info Lxm.srcflagged -> Lic.eq_info Lxm.srcflagged) =
  fun s { it = (lhs,ve) ; src = lxm } -> 
    let lhs = List.map (subst_in_left s ) lhs in
    let newve = subst_in_val_exp s ve in
    { it = (lhs,newve) ; src = lxm }

and subst_in_var_info (s,nb) vi =
  try
    let vi = subst_in_id s vi.var_name_eff in
    let id,clk = vi.var_clock_eff in
    { vi with var_clock_eff =  subst_in_id_id s id, subst_in_clock (s,nb) clk }
  with Not_found -> 
    assert false (* sno : all i/o have been replace ; fresh locs were created *)

and subst_in_left s left =
  match left with
  | LeftVarLic(v,lxm) -> LeftVarLic(subst_in_var_info s v,lxm)
  | LeftFieldLic(left,id,t) -> LeftFieldLic(subst_in_left s left,id,t)
  | LeftArrayLic(left,i, t) -> LeftArrayLic(subst_in_left s left,i, t)
  | LeftSliceLic(left,si,t) -> LeftSliceLic(subst_in_left s left,si,t)

and (subst_in_clock : subst * new_base  -> clock -> clock) =
  fun (s,new_base) clk -> 
    match clk with
      | BaseLic -> new_base
      | ClockVar i -> ClockVar i (* sno *)
      | On ((cc,cv,ct),clk) -> 
         On((cc, subst_in_id_id s cv, ct), subst_in_clock (s,new_base) clk)
           

and (subst_in_val_exp : subst * new_base -> val_exp -> val_exp) =
  fun s ve -> 
    let newve = {
      ve with 
        ve_clk = List.map (subst_in_clock s) ve.ve_clk;
        ve_core =
        match ve.ve_core with
	       | CallByPosLic (by_pos_op, vel) -> 
            let lxm = by_pos_op.src in
            let by_pos_op = by_pos_op.it in
            let vel = List.map (subst_in_val_exp s) vel in
            let by_pos_op = match by_pos_op with
		        | CONST_REF idl -> CONST_REF idl
              | VAR_REF id -> VAR_REF (subst_in_id_id (fst s) id)
              | WHEN(clk) -> WHEN(subst_in_clock s clk)
              | HAT(i) -> HAT(i)
              | PREDEF_CALL _| CALL _ | PRE | ARROW | FBY | CURRENT _ | TUPLE
              | ARRAY | CONCAT | STRUCT_ACCESS _ | ARRAY_ACCES _ | ARRAY_SLICE _ 
              | CONST _ 
                -> by_pos_op
            in
            CallByPosLic(Lxm.flagit by_pos_op lxm, vel)
		        
	       | CallByNameLic(by_name_op, fl) ->
            let fl = List.map (fun (id,ve) -> (id, subst_in_val_exp s ve)) fl in
            CallByNameLic(by_name_op, fl) 
          | Merge(ce,cl) ->
            let cl = List.map (fun (id,ve) -> (id, subst_in_val_exp s ve)) cl in
            Merge(subst_in_val_exp s ce, cl)
    }
    in
    newve

let (mk_loc_subst:subst -> Lic.var_info list -> Lic.var_info list -> subst) =
  fun s l1 l2 -> 
  let l = List.combine l1 l2 in
  List.fold_left
    (fun s (v1,v2) -> add_subst s v1.var_name_eff v2)
    s l

(********************************************************************************)
(** The functions below accumulate 
   (1) the assertions
   (2) the new equations 
   (3) the fresh variables

The fix-point is performed on nodes.
*)

type acc = 
    Lic.val_exp srcflagged list      (* assertions *)
    * (Lic.eq_info srcflagged) list  (* equations *)
    * Lic.var_info list              (* new local vars *)

let (mk_fresh_loc : string -> local_ctx -> var_info -> clock -> var_info) =
  fun label _lctx v c ->
  new_var label ((Lv6Id.to_string v.var_name_eff)) v.var_type_eff (fst v.var_clock_eff, c)

          
let rec get_new_base c_arg c_par =
  match c_arg, c_par with
  | _, BaseLic -> c_arg
  | On(_,c_arg), On(_,c_par) -> get_new_base c_arg c_par
  | _,_ -> assert false (* sno *)
    
          
let (mk_input_subst: local_ctx -> Lxm.t -> var_info list ->
     Lic.val_exp list -> acc -> subst * acc * new_base option) =
  fun lctx lxm vl vel acc ->
  assert(List.length vl = List.length vel);
  let s, acc, new_base_opt =
    List.fold_left2
      (fun (s,(a_acc,e_acc,v_acc),nbase_opt) v ve ->
       (* we create a new var for each node argument to make sure
         that there is no expression in arguments. Is is generally
         useless because of the l2lSplit pass, but, well... *)
        let carg,cpar = match ve.ve_clk, v.var_clock_eff with
          | [c1],(_,c2) ->  c1,c2
          | _ -> assert false
        in
        let new_base = get_new_base carg cpar in
        let nv = mk_fresh_loc "input" lctx v carg  in
        let neq = [LeftVarLic(nv,lxm)],ve in
        let neq = flagit neq lxm in
        let nbase_opt = match nbase_opt with
          | None ->  Some(new_base)
          | Some(b) -> assert(b=new_base); Some(b)
        in
        add_subst s v.var_name_eff nv,
        (a_acc, neq::e_acc, nv::v_acc),
        nbase_opt
      )
      (IdMap.empty, acc, None)
      vl
      vel
  in
  s, acc, new_base_opt

(* Create fresh vars if necessary, ie, if we have no trivial left parts *)
let (mk_output_subst : local_ctx -> Lxm.t -> subst -> var_info list -> Lic.left list ->
                       new_base option-> acc -> subst * acc * new_base) = 
  fun lctx lxm s vl leftl new_base_opt acc ->
  assert(List.length vl = List.length leftl);
  let s,acc,new_base_opt =
    List.fold_left2
      (fun (s,acc,new_base_opt) v left -> 
        match left with
          | LeftVarLic(v2,_lxm) -> add_subst s v.var_name_eff v2, acc,new_base_opt
          | _ -> 
            let clk = Lic.clock_of_left left in
            let nv = mk_fresh_loc "output" lctx v clk in
            let nv_id = nv.var_name_eff in
	         let nve = {
              ve_core = CallByPosLic({it=VAR_REF nv_id;src = lxm },[]);
              ve_typ = [nv.var_type_eff];
              ve_clk = [snd nv.var_clock_eff];
              ve_src = lxm
            }
            in
            let neq = [left], nve in
            let neq = flagit neq lxm in
            (* for equations of the form 
               x.field = n(...);
               we create 
               - a fresh var "_v" 
               - define the equation "x.field = _v;"
               - create the substitution n_output_par/_v
            *)
            let (aa,ae,av) = acc in
            let new_base = get_new_base (Lic.clock_of_left left) (snd v.var_clock_eff) in
            let new_base_opt =
              match new_base_opt with
              | Some x -> assert(x=new_base); Some x
              | None -> Some new_base
            in
            add_subst s v.var_name_eff nv, (aa, neq::ae, nv::av), new_base_opt
      )
      (s,acc,new_base_opt)
      vl 
      leftl
  in
  let new_base = match new_base_opt with
    | Some x -> x
    | None ->
      raise (Lv6errors.Compile_error (
          lxm, "a node ought to have at least one input or one output"))
  in
  s,acc,new_base
  
let rec (expand_eq : local_ctx * acc -> Lic.eq_info Lxm.srcflagged -> local_ctx * acc) =
  fun (lctx, (asserts, eqs, locs)) { src = lxm_eq ; it = eq } ->
    match expand_eq_aux lctx eq with
      | lctx, None -> lctx, (asserts, { src = lxm_eq ; it = eq }::eqs, locs)
      | lctx, Some(nasserts, neqs, nlocs) ->
        (lctx,
         (List.rev_append nasserts asserts, 
          List.rev_append neqs eqs, 
          List.rev_append nlocs locs))

and (expand_eq_aux: local_ctx -> Lic.eq_info -> local_ctx * acc option)=
  fun lctx (lhs,ve) -> 
    match ve.ve_core with
      | CallByPosLic( { it = Lic.CALL node_key ; src = lxm }, vel) ->
        let lctx, node = 
          match LicPrg.find_node lctx.curr_prg node_key.it with 
            | Some node -> lctx, node
            | None -> (* node has not been expanded yet: fix point! *)
              let raw_node =
                match LicPrg.find_node lctx.init_prg node_key.it with 
                  | Some n -> n
                  | None ->
                    prerr_string (
                      "*** "^ (LicDump.string_of_node_key_rec false false node_key.it) ^
                        " not defined.\n*** Defined nodes are:"^ 
                        (String.concat ",\n"  
                           (List.map (fun (nk,_) -> "\""^
                             LicDump.string_of_node_key_rec false false nk^"\"")
                              (LicPrg.list_nodes lctx.init_prg)))
                    );
                    flush stderr;
                    assert false 
              in
              expand_node lctx raw_node 
        in
        let node = match node.def_eff with
          | AbstractLic (Some n) ->  { it = n ; src = lxm }
          | _  -> flagit node lxm
        in            
        if 
          (lctx.keep_node_call node.it.node_key_eff)
        then
          lctx, None
        else
          (match get_node_body lctx node.it with
            | None -> lctx, None
            | Some node_body ->
              let node_eqs = node_body.eqs_eff
              and asserts = node_body.asserts_eff in
              let node = node.it in
              let node_locs = get_locals node in
              let acc = [],[],[] in
              (* rename i/o using the calling context *)
              let s,acc,new_base = mk_input_subst lctx lxm node.inlist_eff vel acc in
              let s,acc,new_base = mk_output_subst lctx lxm s node.outlist_eff
                                                   lhs new_base acc in
              let rec (substitute_clock : clock -> clock) = 
                fun c -> 
                match c with
                | BaseLic -> new_base
                | ClockVar _ -> new_base (* SNO *)
                | On(v,clk) -> On(v, substitute_clock clk)
              in
              let clks = List.map (fun v  -> snd v.var_clock_eff) node_locs in
              (* rename locals with fresh names to avoid clashes among locals *)
              let fresh_locs = List.map2 (mk_fresh_loc "local" lctx) node_locs clks in
              let s = mk_loc_subst s node_locs fresh_locs in
              let fresh_locs = (* substitute the new vars in clocks *)
                List.map (fun v -> 
                  let id,clk = v.var_clock_eff in
                  { v with var_clock_eff =
                             subst_in_id_id s id, subst_in_clock (s,new_base) clk })
                  fresh_locs
              in
              let fresh_locs =
                List.map
                  (fun v ->
                   let id,clk = v.var_clock_eff in
                   Lv6Verbose.exe
                     ~flag:dbg
                     (fun () ->
                      Printf.printf "#EN: remplace the base clk of %s by '%s'\n"
                                    v.var_name_eff
                                    (Lic.string_of_clock new_base));
                   let clk = substitute_clock clk in
                   { v with var_clock_eff = id,clk }
                  )
                  fresh_locs
              in
              let node_eqs = List.map (substitute (s, new_base)) node_eqs in
              let subst_assert x = Lxm.flagit (subst_in_val_exp (s,new_base) x.it) x.src in
              let asserts = List.map subst_assert asserts in              
              let acc = match acc with (a,b,c) ->  (a@asserts,b@node_eqs,c@fresh_locs) in
              lctx, Some acc
          )
      | CallByPosLic (_, _)
      | Merge (_, _) 
      | CallByNameLic (_, _) -> lctx, None

and (expand_assert : local_ctx * acc -> val_exp srcflagged -> local_ctx * acc) =
  fun (lctx, (a_acc,e_acc,v_acc)) ve -> 
    (*   assert(ve); 
       is transformed into
         assert_var=ve;
         assert(assert_var);
       where assert_var is a fresh new local var
    *)
    let lxm = ve.src in
    let ve = ve.it in
    let clk = Lv6Id.of_string "dummy_expand_assert", BaseLic in
    let assert_var = new_var "assert" "assert" Bool_type_eff clk in
    let assert_eq = Lxm.flagit ([LeftVarLic(assert_var,lxm)], ve) lxm in
    let assert_op = Lic.VAR_REF(assert_var.var_name_eff) in
    let nve = { 
      ve_core = CallByPosLic((Lxm.flagit assert_op lxm, []));
      ve_typ = [Bool_type_eff];
      ve_clk = [BaseLic];
      ve_src = lxm
    }
    in
    expand_eq (lctx, ((Lxm.flagit nve lxm)::a_acc, e_acc, assert_var::v_acc)) assert_eq

and (expand_node : local_ctx -> Lic.node_exp -> local_ctx * Lic.node_exp) =
  fun lctx n ->
    match LicPrg.find_node lctx.curr_prg n.node_key_eff with 
      | Some n -> lctx, n
      | None ->
        let lctx, exp_node =
          match n.def_eff with
            | ExternLic 
            | AbstractLic _ -> lctx, n
            | BodyLic b -> 
              let locs = get_locals n in              
              let lctx,acc = List.fold_left expand_eq (lctx, ([],[],locs)) b.eqs_eff in
              let lctx,acc = List.fold_left expand_assert (lctx, acc) b.asserts_eff in
              let (asserts,neqs, nv) = acc in
              let nb = {
                eqs_eff = neqs ; 
                asserts_eff = asserts
              } 
              in
              let res =
                { n with 
                  loclist_eff = Some nv;
                  def_eff = BodyLic nb
                }
              in
              Lv6Verbose.exe ~flag:dbg (fun () ->
                Printf.printf "#EN: '%s'\n"
                  (Lic.string_of_node_key n.node_key_eff));
              lctx, res
            | MetaOpLic -> lctx, n
        in
        { lctx with curr_prg = LicPrg.add_node n.node_key_eff exp_node lctx.curr_prg },
        exp_node

(* exported *)
let (doit : Lic.node_key list -> LicPrg.t -> LicPrg.t) =
  fun node_calls_to_keep inprg ->
    let outprg = LicPrg.empty in
    (* types and constants do not change *)
    let outprg = LicPrg.fold_types  LicPrg.add_type  inprg outprg in
    let outprg = LicPrg.fold_consts LicPrg.add_const inprg outprg in
    let lctx_init = {
      init_prg = inprg;
      curr_prg = outprg;
      keep_node_call = (fun n -> List.mem n node_calls_to_keep);
    }
    in
    (* transform nodes *)
    let  (do_node : Lic.node_key -> Lic.node_exp -> local_ctx -> local_ctx) = 
      fun _nk ne lctx -> 
          fst (expand_node lctx ne)
    in
    let lctx = LicPrg.fold_nodes do_node inprg lctx_init in
    let prg = lctx.curr_prg in
    LicPrg.fold_nodes 
      (fun nk ne acc -> 
        (* remove expanded nodes *)
        if not (List.mem nk node_calls_to_keep) && not (Lic.node_is_extern ne) then 
          LicPrg.del_node nk acc 
        else 
          acc
      )
      prg
      prg
