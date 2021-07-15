(* Time-stamp: <modified the 29/08/2019 (at 15:41) by Erwan Jahier> *)


open Lxm
open Lic

let dbg = (Lv6Verbose.get_flag "enumasbool")

type target = I (*int*) | BA (* Boolean array *)

(* exported *)
let  (doit : target -> LicPrg.t -> LicPrg.t) =
  fun target inprg -> 
  
  let rec (do_var_info  : Lic.var_info -> Lic.var_info) =
    fun vi ->
    { vi with var_type_eff = do_type vi.var_type_eff  }

  and (do_left : Lic.left -> Lic.left) =
    fun l ->
    match l with
    | LeftVarLic(vi,lxm)   -> LeftVarLic(do_var_info vi,lxm)
    | LeftFieldLic(l,id,t) -> LeftFieldLic(do_left l,id, do_type t)
    | LeftArrayLic(l,i,t)  -> LeftArrayLic(do_left l,i,  do_type t)
    | LeftSliceLic(l,si,t) -> LeftSliceLic(do_left l,si, do_type t)

  and (do_type :Lic.type_ -> Lic.type_) =
    fun t ->
    match target, t  with
    | BA,Enum_type_eff(_n,ll) -> Array_type_eff(Bool_type_eff,List.length ll)
    | I ,Enum_type_eff(_,_) -> Int_type_eff
    | _ ,Bool_type_eff
    | _ ,Int_type_eff
    | _ ,Real_type_eff
    | _ ,External_type_eff _
    | _ ,TypeVar _  -> t
    | _ ,Abstract_type_eff(id,t) -> Abstract_type_eff(id, do_type t)
    | _ ,Array_type_eff(t,i) -> Array_type_eff(do_type t,i)
    | _ ,Struct_type_eff(n,l) ->
       let l = List.map (fun (id, (t, opt)) ->  (id, (do_type t, opt))) l in
       Struct_type_eff(n,l)
                      
  and (do_eq : Lic.eq_info srcflagged -> Lic.eq_info srcflagged ) =
    fun { src = lxm_eq ; it = (left_list, ve) } ->
    let ve = do_val_exp ve in
    let left_list = List.map do_left left_list in 
    { src = lxm_eq ; it = (left_list, ve) }

  and (do_const : Lic.const -> Lic.const) =
    fun c ->
    match c with
    | Enum_const_eff (s,Enum_type_eff(_,ll)) ->
       if target = BA then
         let xl =
           List.map (fun x -> Bool_const_eff(if x=s then true else false)) ll
         in
         Array_const_eff(xl,Array_type_eff(Bool_type_eff,List.length ll))
       else
         let i = Lv6util.pos_in_list 0 s ll in
         Int_const_eff (string_of_int i)
    | Enum_const_eff (n,t) ->  Enum_const_eff (n,do_type t)
    | Abstract_const_eff(id, t, c, b) ->
       Abstract_const_eff(id, do_type t, do_const c, b)
    | Struct_const_eff(l,t) ->
       let l = List.map (fun (id,c) -> id, do_const c) l in
       Struct_const_eff(l,do_type t)
                       
    | Array_const_eff(cl,t) -> Array_const_eff(List.map do_const cl, do_type t)
    | Tuple_const_eff(cl) -> Tuple_const_eff(List.map do_const cl)
    | Extern_const_eff(id,t) -> Extern_const_eff(id, do_type t) 

    | Bool_const_eff(_)
    | Int_const_eff(_)
    | Real_const_eff(_) -> c

  and (do_val_exp: val_exp -> val_exp) =
    fun ve ->
    let ve_core =
      match ve.ve_core with
      | Merge(ce,cl) -> (
        let cl = 
          List.fold_left
            (fun (ncl) ({src=lxm;it=c}, ve) -> 
             let ve = do_val_exp ve in
             (({src=lxm;it= (* do_const*) c}, ve)::ncl)
            ) 
            [] cl 
        in
        Merge(ce,cl)
      )
      | CallByNameLic(op, fl) -> (
        let fl = List.fold_left (fun (nfl) (id,ve) -> ((id,do_val_exp ve)::nfl)) 
                                ([]) (List.rev fl) 
        in
        CallByNameLic(op, fl)
      )
      | CallByPosLic (op, vel) -> (
        let vel =
          List.fold_left (fun vel ve -> (do_val_exp ve)::vel) [] (List.rev vel)
        in
        let ec =  Lv6MainArgs.global_opt.Lv6MainArgs.ec in
        let op_it,vel =
          match op.it, ec with
          | Lic.CONST c,_ -> Lic.CONST (do_const c), vel
          | Lic.CONST_REF idl, true -> (
            match target, LicPrg.find_const inprg idl with
            | _, None -> op.it,vel
            | BA, Some (Enum_const_eff (s,Enum_type_eff(_,ll))) ->
               let f x =
                 let c = CONST(Bool_const_eff(if x=s then true else false)) in
                 let c = { it = c ; src = op.src } in
                 { ve with ve_core = CallByPosLic(c, []); ve_typ = [Bool_type_eff] }
               in
               let xl = List.map f ll in
               Lic.ARRAY, xl
            | I, Some (Enum_const_eff (s,Enum_type_eff(_,ll))) -> (
              let i = Lv6util.pos_in_list 0 s ll in
              Lic.CONST (Int_const_eff (string_of_int i)), vel
            )
            | _,_ -> op.it, vel
          )
          | _,_ -> op.it, vel
        in
        let op = { op with it = op_it } in
        CallByPosLic(op, vel)
	   )
    in 
    { ve with
      ve_core = ve_core;
      ve_typ = List.map do_type ve.ve_typ;
    }

  and (do_val_exp_flag:val_exp srcflagged -> val_exp srcflagged) =
    fun ve_f -> 
    let ve = do_val_exp ve_f.it in
    { ve_f with it = ve }

  and (do_node : Lic.node_exp -> Lic.node_exp) =
    fun n ->
    let def_eff = 
    match n.def_eff with
    | ExternLic | MetaOpLic | AbstractLic _ -> n.def_eff
    | BodyLic b ->
       let eqs = List.map (fun eq -> do_eq eq) b.eqs_eff  in
       let ass = List.map (fun ve -> do_val_exp_flag ve) b.asserts_eff in
       BodyLic{asserts_eff=ass; eqs_eff=eqs}
    in
    { n with
      inlist_eff = List.map do_var_info n.inlist_eff;
      outlist_eff = List.map do_var_info n.outlist_eff;
      loclist_eff = (match n.loclist_eff with
                     | None -> None
                     | Some l -> Some(List.map do_var_info l));
      def_eff = def_eff;
    }

  in (* back to doit *)
  let outprg = LicPrg.empty in
  (* types and constants do not change *)
  let outprg =
    LicPrg.fold_types (fun k t acc -> LicPrg.add_type k (do_type t) acc)
                      inprg outprg
  in
  let outprg =
    LicPrg.fold_consts (fun k c acc -> LicPrg.add_const k (do_const c) acc)
                       inprg outprg
  in
  (* transform nodes *)
  let (doit_node : Lic.node_key -> Lic.node_exp -> LicPrg.t -> LicPrg.t) = 
    fun nk ne outprg -> 
    Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf "#DBG: L2lExpandEnum '%s'\n"
                                                     (Lic.string_of_node_key nk));
    let ne = do_node ne in
    LicPrg.add_node nk ne outprg
  in
  let outprg = LicPrg.fold_nodes doit_node inprg outprg in
  outprg
