(* Time-stamp: <modified the 29/08/2019 (at 16:08) by Erwan Jahier> *)


open Lxm
open Lic

let dbg = (Lv6Verbose.get_flag "noalias")

module StringMap = Map.Make(String)

type subst = Lic.var_info StringMap.t
type acc = subst * Lic.eq_info srcflagged list (* equations *)

let (apply_subst : string -> subst -> string) =
  fun id s -> 
  try
    let nv = StringMap.find id s in
    Lv6Verbose.exe
      ~flag:dbg (fun() -> Printf.printf "#DBG:    L2lRemoveAlias.apply_subst '%s->%s'\n"
                id nv.var_name_eff);
                      
  nv.var_name_eff
    with Not_found -> id
  
let rec (do_clock  : subst -> Lic.clock -> Lic.clock) =
  fun s clk ->
  match clk with
  | On((cc,id,t),sclk) -> On((cc,apply_subst id s,t), do_clock s sclk)
  | e -> e
                               
let (do_var_info  : subst -> Lic.var_info -> Lic.var_info) =
  fun s v ->
  try StringMap.find v.var_name_eff s 
  with Not_found -> 
    let id,clk = v.var_clock_eff in
    let clk = do_clock s clk in 
    { v with var_clock_eff = apply_subst id s, clk }

let add_subst (x:Lv6Id.t) (nv:Lic.var_info) (s:subst) =
  let nv = do_var_info s nv in
  let s = StringMap.mapi (fun _k v -> if v.var_name_eff = x then nv else v) s in
  Lv6Verbose.exe
    ~flag:dbg (fun() -> Printf.printf "#DBG:   L2lRemoveAlias.add_subst '%s->%s'\n"
                                      x nv.var_name_eff);
  StringMap.add x nv s
    
let rec (do_eq_pass1 : Lic.node_exp -> Lic.eq_info srcflagged -> acc -> acc) =
  fun node_exp { src = _lxm_eq ; it = (left_list, ve) } acc ->
    match left_list, ve with
    | [LeftVarLic(vi,_)],
      { ve_core = CallByPosLic(
            { it = TUPLE ;_},
            [ { ve_core = CallByPosLic({ it=VAR_REF(id) ;_},_) ;_} ]);_ } 
    | [LeftVarLic(vi,_)],
      { ve_core = CallByPosLic({ it=VAR_REF(id);_ },_);_ } ->
      let s, eqs = acc in
      let v_id = match LicPrg.find_var id node_exp with
          Some vi -> vi
        | None -> assert false
      in
      if List.mem vi node_exp.outlist_eff then
        s,  { src = _lxm_eq ; it = (left_list, ve) } :: eqs
      else 
        add_subst vi.var_name_eff v_id s, eqs
    | _,_  ->
      let s, eqs = acc in
      (*      let ve = do_val_exp licprg ve s in *)
      s, { src = _lxm_eq ; it = (left_list, ve) }::eqs

and (do_eq_pass2 : subst -> Lic.eq_info srcflagged -> Lic.eq_info srcflagged ) =
  fun s { src = lxm_eq ; it = (left_list, ve) } ->
    let ve = do_val_exp ve s in
    let left_list = List.map (do_left s) left_list in
    { src = lxm_eq ; it = (left_list, ve) }

and (do_left: subst -> left -> left) =
  fun s ->
    function
    | LeftVarLic   (var_info,lxm) -> LeftVarLic (do_var_info s var_info,lxm)
    | LeftFieldLic (left,id,t) -> LeftFieldLic (do_left s left,id,t)
    | LeftArrayLic (left,i,t) -> LeftArrayLic (do_left s left,i,t)
    | LeftSliceLic (left,si,t) -> LeftSliceLic (do_left s left,si,t) 


and (do_val_exp: val_exp -> subst -> val_exp) =
  fun ve s ->
    let ve_core =
      match ve.ve_core with
      | Merge(ce,cl) -> (
          let cl = 
            List.fold_left
              (fun (ncl) (c, ve) -> 
                 let ve = do_val_exp ve s in
                 ((c, ve)::ncl)
              ) 
              ([]) 
              cl 
          in
          let ce = do_val_exp ce s in
          Merge(ce,cl)
        )
      | CallByNameLic(op, fl) -> (
          let fl = List.fold_left
              (fun (nfl) (id,ve) -> 
                 let nf = do_val_exp ve s in
                 ((id,nf)::nfl)
              ) 
              ([]) 
              (List.rev fl) 
          in
          CallByNameLic(op, fl)
        )
      | CallByPosLic (op, vel) -> (
          let op = match op with
            | { src=lxm; it=VAR_REF id } -> { src=lxm ; it=VAR_REF (apply_subst id s) }
            | { src=lxm; it=WHEN clk } ->  { src=lxm; it=WHEN (do_clock s clk) }
            | {  src=lxm; it=CURRENT (Some (pn,clk_cc)) } ->
              { src=lxm; it=CURRENT (Some (pn, (apply_subst clk_cc s))) }

            | x -> x
          in
          let vel =
            List.fold_left (fun vel ve -> (do_val_exp ve s)::vel) [] (List.rev vel)
          in
          CallByPosLic(op, vel)
	     )
    in 
    { ve with
      ve_core = ve_core;
      ve_clk = List.map (do_clock s) ve.ve_clk
    }

and (do_val_exp_flag:val_exp srcflagged -> subst -> val_exp srcflagged) =
  fun ve_f s -> 
    let ve = do_val_exp ve_f.it s in
    { ve_f with it = ve }

and (do_node : LicPrg.t -> Lic.node_exp -> Lic.node_exp) =
  fun _licprg n -> 
    match n.def_eff with
    | ExternLic | MetaOpLic | AbstractLic _ -> n
    | BodyLic b ->
      let s, neqs = List.fold_left 
          (fun acc eq -> do_eq_pass1 n eq acc) 
          (StringMap.empty,[]) 
          (List.rev b.eqs_eff)
      in
      let neqs = List.map (do_eq_pass2 s) neqs in
      let nass = List.map (fun ve -> do_val_exp_flag ve s) b.asserts_eff in
      let nlocs = match n.loclist_eff with
        | None -> None
        | Some vl ->
          let vl = List.filter (fun v -> not (StringMap.mem v.var_name_eff s)) vl in
          Some(List.map (do_var_info s) vl)
      in
      { n with 
        def_eff = BodyLic  { 
            eqs_eff = neqs; 
            asserts_eff = nass;
          };
        loclist_eff =nlocs;
        inlist_eff  = List.map (do_var_info s) n.inlist_eff;
        outlist_eff = List.map (do_var_info s) n.outlist_eff;
      }

          
(* exported *)
let (doit : LicPrg.t -> LicPrg.t) =
  fun inprg -> 
    let outprg = LicPrg.empty in
  (* types and constants do not change *)
    let outprg = LicPrg.fold_types  LicPrg.add_type  inprg outprg in
    let outprg = LicPrg.fold_consts LicPrg.add_const inprg outprg in
    
  (* transform nodes *)
    let (doit_node : Lic.node_key -> Lic.node_exp -> LicPrg.t -> LicPrg.t) = 
      fun nk ne outprg -> 
        Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf "#DBG: L2lRemoveAlias '%s'\n"
            (Lic.string_of_node_key nk));
        let ne = do_node inprg ne in
        LicPrg.add_node nk ne outprg
    in
    let outprg = LicPrg.fold_nodes doit_node inprg outprg in
    outprg
