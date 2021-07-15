(** Time-stamp: <modified the 29/08/2019 (at 16:07) by Erwan Jahier> *)

open Lxm
open Lic
 
let dbg =  (Lv6Verbose.get_flag "woi")


(********************************************************************************)
(** The functions below accumulate 
   (1) the new assertions
   (2) the new equations 
   (3) the fresh variables
   (4) the table mapping enumerated clocks to new boolean clock
*)

type clock_tbl = ((Lv6Id.long * Lv6Id.t), Lic.var_info) Hashtbl.t (* use Map.t ? *)
type acc = 
      Lic.var_info list           (* new local vars *)
    * Lic.eq_info srcflagged list (* equations *)
    * Lic.val_exp srcflagged list (* assertions *)
    * clock_tbl

let new_var (cc,cv) type_eff clock_eff clk_tbl = 
   try Hashtbl.find clk_tbl (cc,cv), true
   with Not_found ->
     let str = (snd cc) ^ "_" ^ cv in
     let str2 = (snd cc) ^ "(" ^ cv ^")" in
     let id = Lv6Id.of_string (FreshName.local_var (str)) in 
     Lv6Verbose.exe
       ~flag:dbg
       (fun() -> Printf.printf
                   "L2lWhenOnId: '%s' not found; create %s\n" str2 id);
     let var =
       { 
         var_name_eff   = id;
         var_nature_eff = AstCore.VarLocal;
         var_number_eff = -1;
         var_type_eff   = type_eff;
         var_clock_eff  = id,clock_eff;
       } 
     in
     Hashtbl.add clk_tbl (cc,cv) var;
     var,false

(********************************************************************************)
(* The one that performs the real work, namely:

- if it hasn't been created yet
   - invent a fresh var name NV, 
   - generate the equation defining NV (NV=(Idle=st);)
*)
let (gen_ident : Lxm.t -> Lv6Id.long -> Lv6Id.t -> type_ -> Lic.clock -> acc -> 
                  var_info * acc ) =
   fun lxm cc cv ct clk (vl,eql,al,tbl) -> (* On(Idle, st, enum_t)  *)
   let nv, already_done = new_var (cc,cv) Bool_type_eff clk tbl in
     let ve1 = {
       ve_typ = [ct];
       ve_clk = [clk]; 
       ve_core = CallByPosLic(Lxm.flagit (Lic.CONST_REF cc) lxm,[]);
       ve_src = lxm
     }
     in
     let ve2 = {
       ve_typ = [ct];
       ve_clk = [clk]; 
       ve_core = CallByPosLic(Lxm.flagit (Lic.VAR_REF cv) lxm,[]);
       ve_src = lxm	
     }
     in 
     let ve = (* Idle=st *){
       ve_typ = [Bool_type_eff];
       ve_clk = [clk]; 
       ve_core = 
         CallByPosLic({it=Lic.PREDEF_CALL({it=(("Lustre","eq"),[]);src=lxm});src=lxm}, 
                      [ve1;ve2]);
       ve_src = lxm
     }
     in
     if already_done then
       nv, (vl,eql,al,tbl)
     else
       let new_eq = (* NV=(Idle=st) *) {src=lxm;it=[LeftVarLic (nv, lxm)], ve} in
       nv,  (nv::vl,new_eq::eql,al,tbl)
        
(********************************************************************************)
let rec update_clock : Lxm.t -> acc -> clock -> clock * acc = 
  fun lxm acc clk ->
  match clk with
  | BaseLic | ClockVar _ -> clk, acc
  | On((cc,cv,ct),sclk) ->
     let sclk, acc = update_clock lxm acc sclk in
     let (_,_,_,clk_tbl) = acc in
     try 
       let nv = Hashtbl.find clk_tbl (cc,cv) in
       On((("Lustre","true"),nv.var_name_eff, Bool_type_eff), sclk), acc
     with Not_found ->
       if cc = ("Lustre","true") || cc = ("Lustre","false") then 
         On((cc,cv,ct),sclk), acc
       else
         let nv, acc = gen_ident lxm cc cv ct sclk acc in
         On((("Lustre","true"),nv.var_name_eff, Bool_type_eff), sclk), acc

let update_clock_list : Lxm.t -> clock list * acc -> clock -> clock list * acc =
  fun lxm (res,acc) clk ->
  let clk, acc = update_clock lxm acc clk in
  clk::res, acc

                                                                         
let update_var_info : Lxm.t -> acc -> var_info -> var_info * acc = 
  fun lxm acc vi ->
  let (id, clk) = vi.var_clock_eff in
  let clk, acc = update_clock lxm acc clk in
  Lv6Verbose.exe
    ~flag:dbg
    (fun() -> Printf.printf
                "#DBG: L2lWhenOnId;  update_var_info %s\n" id);
  { vi with var_clock_eff = id, clk }, acc

let update_var_info_list : Lxm.t -> var_info list * acc -> var_info ->
                           var_info list * acc = 
  fun lxm (res,acc) vi ->
  let vi, acc = update_var_info lxm acc vi in
  vi::res, acc

 let rec update_left : acc -> left -> left * acc = 
   fun acc l -> 
   match l with
   | LeftVarLic(vi,lxm) ->
      let vi, acc = update_var_info lxm acc vi in
      LeftVarLic(vi,lxm), acc
   | LeftFieldLic(l,id,t) -> 
      let l, acc = update_left acc l in
      LeftFieldLic(l,id,t),acc
   | LeftArrayLic(l,i,t) -> 
      let l, acc = update_left acc l in
      LeftArrayLic(l,i,t), acc
   | LeftSliceLic(l,si,t) -> 
      let l, acc = update_left acc l in
      LeftSliceLic(l,si,t), acc

 let update_left_list : left list * acc -> left -> left list * acc = 
   fun (ll,acc) l -> 
   let l, acc = update_left acc l in
   l::ll, acc

                              
(********************************************************************************)
(* All the remaining are admnistrative functions *)
   
let rec (do_eq : LicPrg.t -> Lic.eq_info srcflagged -> acc -> acc) =
  fun licprg { src = lxm_eq ; it = (left_list, ve) } acc -> 
    let ve, (nv,neqs,nass,tbl) = do_val_exp licprg ve acc in
    let left_list,_acc = List.fold_left update_left_list ([],acc) left_list in
    let left_list = List.rev left_list in
    nv, { src = lxm_eq ; it = (left_list, ve) }::neqs, nass,tbl
      
and (do_val_exp: LicPrg.t -> val_exp -> acc -> val_exp * acc) =
  fun licprg ve acc ->
    let ve_core, acc =
      match ve.ve_core with
        | Merge(ce,cl) -> (
          let cl,acc = 
            List.fold_left
              (fun (ncl,acc) (c, ve) -> 
                let ve, acc = do_val_exp licprg ve acc in
                ((c, ve)::ncl), acc) 
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
          | WHEN (On((cc,cv,Bool_type_eff),clk)) -> (* nothing to do in this case *)
             let clk, acc = update_clock op.src acc clk in              
             let new_op = WHEN (On((cc, cv, Bool_type_eff),clk)) in
             let op = Lxm.flagit new_op op.src in
             CallByPosLic(op, vel), acc
          | WHEN (On((cc,cv,ct),clk)) ->
             let clk, acc = update_clock op.src acc clk in              
             let nv, acc = gen_ident op.src cc cv ct clk acc in
             let true_cc = "Lustre","true" in
             let new_op = WHEN (On((true_cc, nv.var_name_eff, Bool_type_eff),clk)) in
             let op = Lxm.flagit new_op op.src in
             CallByPosLic(op, vel), acc
          | _ ->
             CallByPosLic(op, vel), acc
	     )
    in
    let lxm = lxm_of_val_exp ve in
    let ve_clk, acc = List.fold_left (update_clock_list lxm) ([],acc) ve.ve_clk in
      { ve with
        ve_core = ve_core;
        ve_clk  = List.rev ve_clk;
      },acc

and (do_val_exp_flag:  LicPrg.t -> val_exp srcflagged -> acc -> 
     val_exp srcflagged * acc) =
  fun licprg ve_f acc -> 
    let ve, acc = do_val_exp licprg ve_f.it acc in
    { ve_f with it = ve }, acc

and (do_node : LicPrg.t -> Lic.node_exp -> Lic.node_exp) =
  fun licprg n -> 
  let tbl = Hashtbl.create 0 in
  let aux licprg n acc =
    match n.def_eff with
      | ExternLic | MetaOpLic | AbstractLic _ -> n,acc
      | BodyLic b ->
        let acc = List.fold_left 
                    (fun acc eq -> do_eq licprg eq acc)
                    acc
                    (List.rev b.eqs_eff)
        in
        let acc = List.fold_left 
          (fun acc ve -> 
            let ve,(nv,neq,nass,tbl) = do_val_exp_flag licprg ve acc in
            (nv,neq,ve::nass,tbl)
          )
          acc
          b.asserts_eff 
        in
        let uvi = update_var_info_list n.lxm in
        let inlist_eff,acc  = List.fold_left uvi ([],acc) n.inlist_eff in
        let outlist_eff,acc = List.fold_left uvi ([],acc) n.outlist_eff in
        let locs = match n.loclist_eff with Some x -> x | None -> [] in
        let loclist_eff,acc = List.fold_left uvi ([],acc) locs in        
        let (nv,neqs,nass,_tbl) = acc in
        let nlocs = List.rev_append nv loclist_eff in
        { n with 
          def_eff = BodyLic { eqs_eff = neqs; asserts_eff = nass };
          inlist_eff  = List.rev inlist_eff;
          outlist_eff = List.rev outlist_eff;
          loclist_eff = Some (List.rev nlocs)
        },acc
  in
  let n,_acc = aux licprg n  ([],[],[],tbl) in
  n

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
        Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf "#DBG: L2lWhenOnId '%s'\n"
            (Lic.string_of_node_key nk));
        let ne = do_node inprg ne in
        LicPrg.add_node nk ne outprg
    in
    let outprg = LicPrg.fold_nodes doit_node inprg outprg in
    outprg
