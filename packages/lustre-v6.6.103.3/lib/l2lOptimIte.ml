(** Time-stamp: <modified the 29/08/2019 (at 15:44) by Erwan Jahier> *)

open Lxm
open Lic
 
let dbg =  (Lv6Verbose.get_flag "oite")


(********************************************************************************)
(** The functions below accumulate 
   (1) the assertions
   (2) the new equations 
   (3) the fresh variables

*)

type acc = 
      Lic.var_info list           (* new local vars *)
    * Lic.eq_info srcflagged list (* equations *)
    * Lic.val_exp srcflagged list (* assertions *)

let new_var = FreshName.var_info

(********************************************************************************)
let  (is_memory_less_and_safe_val_exp : LicPrg.t -> Lic.val_exp -> bool) =
  fun licprg ve ->
    L2lCheckMemSafe.is_safe_val_exp       licprg ve &&
    L2lCheckMemSafe.is_memoryless_val_exp licprg ve

let (clock_of_cond: val_exp -> bool -> acc -> clock * acc) =
  fun ve b acc ->
    let ve_clk = match ve.ve_clk with [clk] -> clk | _ -> assert false in
    let b = if b then "true" else "false" in
    let cc = "Lustre", b in
    let lxm = Lic.lxm_of_val_exp ve in
    let cv,acc = match ve with
      | { ve_core= CallByPosLic({it=VAR_REF id;_},[]) ;_} -> id,acc
      | _ -> 
        let nv = new_var "clk__of_ite" Bool_type_eff ("dummy",ve_clk) in
        let eq = {src=lxm;it=[LeftVarLic (nv, lxm)], ve} in
        let v,eqs,ass = acc in
        let acc = nv::v,eq::eqs,ass in
          nv.var_name_eff, acc
    in
    On((cc,cv,Bool_type_eff), List.hd ve.ve_clk), acc


let  (val_exp_when_clk : Lic.val_exp -> Lic.clock -> Lic.val_exp) =
  fun ve clk ->
(* return "ve when clk" *)
    let lxm = Lic.lxm_of_val_exp ve in
    { ve with 
      ve_clk = List.map (fun _ -> clk) ve.ve_clk; (* *)
      ve_core = CallByPosLic({ src=lxm; it=Lic.WHEN clk },[ve]);
    }
      
let (gen_merge : Lxm.t -> Lic.val_exp -> Lic.val_exp -> Lic.val_exp -> acc ->
     Lic.val_exp_core * acc ) =
  fun lxm cond ve1 ve2 acc ->
    (* this is the core of the module *)
    let true_cst  = { it=Bool_const_eff true ; src = lxm } in
    let false_cst = { it=Bool_const_eff false; src = lxm } in
    let clk_true,acc  = clock_of_cond cond true  acc in
    let clk_false,acc = clock_of_cond cond false acc in
    let ve1 = val_exp_when_clk ve1 clk_true in
    let ve2 = val_exp_when_clk ve2 clk_false in
    Merge(cond, [(true_cst,  ve1); (false_cst, ve2)]), acc


let is_var_ref ve = 
  match ve.ve_core with 
    | CallByPosLic({it=VAR_REF _;_},[]) -> true 
    | _ -> false
      

(* All the remaining are admnistrative functions *)
   
let rec (do_eq : LicPrg.t -> Lic.eq_info srcflagged -> acc -> acc) =
  fun licprg { src = lxm_eq ; it = (left_list, ve) } acc -> 
    let ve, (nv,neqs,nass) = do_val_exp licprg ve acc in
    nv, { src = lxm_eq ; it = (left_list, ve) }::neqs, nass
      
and (do_val_exp: LicPrg.t -> val_exp -> acc -> val_exp * acc) =
  fun licprg ve acc ->
    let ve_core, acc =
      match ve.ve_core with
        | Merge(ce,cl) -> (
          let cl,acc = 
            List.fold_left
              (fun (ncl,acc) (c, ve) -> 
                let ve, acc = do_val_exp licprg ve acc in
                ((c, ve)::ncl), acc
              ) 
              ([],acc) 
              cl 
          in
          Merge(ce,cl),acc
        )
        | CallByNameLic(op, fl) -> (
          let fl,acc = List.fold_left
            (fun (nfl,acc) (id,ve) -> 
              let nf, acc = do_val_exp licprg ve acc in
              ((id,nf)::nfl), acc
            ) 
            ([],acc) 
            fl 
          in
          CallByNameLic(op, fl), acc
        )
        | CallByPosLic (op, vel) -> (
          let vel, acc = List.fold_left
            (fun (vel,acc) ve -> 
              let ve, acc = do_val_exp licprg ve acc in
              (ve::vel), acc
            )
            ([],acc) 
            (List.rev vel)
          in          
	       match op.it with
            | CALL({src=_if_lxm; it=("Lustre","if"),[]}) 
            | PREDEF_CALL({src=_if_lxm; it=("Lustre","if"),[]}) -> (
              match vel with
                | [ cond ; ve1; ve2] -> 
                  if is_memory_less_and_safe_val_exp licprg ve1 &&  
                    is_memory_less_and_safe_val_exp licprg ve2 &&
                    is_var_ref cond (* exclude "if true then ..." from this optim *)
                  then 
                    gen_merge op.src cond ve1 ve2 acc
                  else
                    CallByPosLic(op, vel), acc
                | _ -> assert false (* SNO *)
            )
            | _ -> CallByPosLic(op, vel), acc
	     )
    in 
    { ve with ve_core = ve_core },acc

and (do_val_exp_flag:  LicPrg.t -> val_exp srcflagged -> acc -> val_exp srcflagged * acc) =
  fun licprg ve_f acc -> 
    let ve, acc = do_val_exp licprg ve_f.it acc in
    { ve_f with it = ve }, acc

and (do_node : LicPrg.t -> Lic.node_exp -> Lic.node_exp) =
  fun licprg n -> 
    match n.def_eff with
      | ExternLic | MetaOpLic | AbstractLic _ -> n
      | BodyLic b ->
        let acc = List.fold_left 
          (fun acc eq -> do_eq licprg eq acc) 
          ([],[],[]) 
          (List.rev b.eqs_eff)
        in
        let acc = List.fold_left 
          (fun acc ve -> 
            let ve,(nv,neq,nass) = do_val_exp_flag licprg ve acc in
            (nv,neq,ve::nass)
          )
          acc
          b.asserts_eff 
        in
        let (nv,neqs,nass) = acc in
        let nlocs = match n.loclist_eff with
          | None -> nv (* SNO, but eats no bread *)
          | Some v -> List.rev_append nv v
        in
        { n with 
          def_eff = BodyLic  { 
            eqs_eff = neqs; 
            asserts_eff = nass;
          };
          loclist_eff = Some nlocs;
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
        Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf "#DBG: L2lOptimIte '%s'\n"
            (Lic.string_of_node_key nk));
        let ne = do_node inprg ne in
        LicPrg.add_node nk ne outprg
    in
    let outprg = LicPrg.fold_nodes doit_node inprg outprg in
    outprg
