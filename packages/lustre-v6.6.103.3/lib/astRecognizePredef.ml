(* Time-stamp: <modified the 22/08/2017 (at 14:57) by Erwan Jahier> *)


let (get_predef : Lv6Id.idref -> AstPredef.op option) =
  fun idref -> 
  let get_op () = (* XXX if a user node is named "plus", this does not work *)
      try Some (AstPredef.string_to_op (Lv6Id.to_string (Lv6Id.name_of_idref idref)))
      with Not_found -> None
    in
    match Lv6Id.pack_of_idref idref with
      | None -> None
(*         get_op () (* The Lustre package is «use»d by default *)  *)
      | Some p -> if (Lv6Id.pack_name_to_string p) = "Lustre" then get_op () else None

open AstV6
open AstCore
open Lxm

let flag f x_flg = Lxm.flagit (f x_flg.it) x_flg.src

let flag2 f x_flg = Lxm.flagit (f x_flg) x_flg.src

let fopt f = function None -> None | Some x -> Some (f x)


(* just a tedious recursive traversal of the syntax tree, replacing idref
   that match predef op with the Predef constructor *)
(* exported *)
let rec (f : AstV6.t -> AstV6.t) = function
  | PRPackBody(sl,pb) -> PRPackBody(sl, r_packbody pb)
  | PRPack_or_models(sl,pml) -> PRPack_or_models(sl,List.map r_pack_or_model pml)

and r_pack_or_model = function   
  | NSPack(pi) -> NSPack(flag r_pack_info pi)
  | NSModel(mi) -> NSModel(flag r_model_info mi)

and r_pack_info pi = { pi with pa_def = r_pack_def pi.pa_def } 

and r_model_info mi = 
  { mi with 
    mo_needs = List.map (flag r_static_param) mi.mo_needs;
    mo_provides = r_item_info_flg_list mi.mo_provides;
    mo_body = r_packbody  mi.mo_body;
  }

and r_pack_def = function
  | PackGiven(pg) -> PackGiven(r_pack_given pg)
  | PackInstance(pi) -> PackInstance(r_pack_instance pi)

and r_pack_given pg = { 
  pg with
    pg_provides = r_item_info_flg_list pg.pg_provides;
    pg_body = r_packbody pg.pg_body;
}

and r_pack_instance pi = { pi with pi_args = List.map r_by_name_static_arg pi.pi_args }

and r_static_param sp = sp 

and r_by_name_static_arg (id,arg) = 
  let arg_it = 
    match arg.it with
      | StaticArgLv6Id(idref) -> (
        match get_predef idref with
	       | None -> StaticArgLv6Id idref
	       | Some predef -> StaticArgNode (Predef_n (flagit predef arg.src))
      )
      | StaticArgConst(ve) -> StaticArgConst(r_val_exp ve)
      | StaticArgType(te) -> StaticArgType(te)
      | StaticArgNode(by_pos_op) -> StaticArgNode(r_by_pos_op (flagit by_pos_op arg.src))
  in
  id, Lxm.flagit arg_it arg.src

and r_static_arg arg = 
  match arg.it with
    | StaticArgLv6Id(idref) -> (
      match get_predef idref with
	     | None -> StaticArgLv6Id idref
	     | Some predef -> StaticArgNode (Predef_n (flagit predef arg.src))
    )
    | StaticArgConst(ve) -> StaticArgConst(r_val_exp ve)
    | StaticArgType(te) -> StaticArgType(te)
    | StaticArgNode(by_pos_op) -> StaticArgNode(r_by_pos_op (flagit by_pos_op arg.src))

and r_by_pos_op arg =
  match arg.it with
    | CALL_n { src=lxm;it=(idref,sargs) } -> (
      match get_predef idref with
	     | None -> CALL_n { src=lxm;it= r_node_exp (idref,sargs) }
	     | Some op -> assert (sargs=[]); Predef_n (flagit op arg.src)
    )
    | IDENT_n(idref) -> (
      match get_predef idref with
	     | None -> IDENT_n(idref)
	     | Some op -> Predef_n (flagit op arg.src)
    )
    | ARRAY_ACCES_n(val_exp) -> ARRAY_ACCES_n(r_val_exp val_exp)
    | ARRAY_SLICE_n(slice_info) -> ARRAY_SLICE_n(r_slice_info slice_info)

    | x -> x

and r_node_exp (idref, sargs) =
  (idref, List.map (flag2 r_static_arg) sargs)

    
and r_slice_info si = {
  si_first = r_val_exp si.si_first;
  si_last  = r_val_exp si.si_last;
  si_step  = fopt r_val_exp si.si_step;
}

and r_val_exp = function
  | CallByPos (by_pos_op, Oper vel) -> 
    CallByPos(flag2 r_by_pos_op by_pos_op, Oper (List.map r_val_exp vel))
  | CallByName(by_name_op, args) -> 
    CallByName(by_name_op, List.map (fun (id, ve) -> id, r_val_exp ve) args)
  | Merge_n (ec,cl) -> 
    let cl = List.map (fun (id,ve) ->  (id, r_val_exp ve)) cl in
    Merge_n (ec,cl) 
  | Merge_bool_n(id, t, f) -> Merge_bool_n(id, r_val_exp t, r_val_exp f) 
    
and r_item_info_flg_list = function
  | None -> None
  | Some iil -> Some (List.map (flag r_item_info) iil)

and r_item_info = function
  | ConstInfo ci -> ConstInfo(r_const_info ci)
  | TypeInfo  ti -> TypeInfo (r_type_info ti)
  | NodeInfo  ni -> NodeInfo (r_node_info ni)

and r_const_info = function
  | ExternalConst(id,te,ve_opt) -> ExternalConst(id,te, fopt r_val_exp ve_opt)
  | EnumConst(id,te) -> EnumConst(id,te)
  | DefinedConst(id,te,ve) -> DefinedConst(id,te, r_val_exp ve)

and r_type_info = function 
  | ExternalType(id) -> ExternalType(id)
  | AliasedType(id,te) -> AliasedType(id,te)
  | EnumType(id,te) -> EnumType(id,te)
  | StructType(sti) -> StructType(r_struct_type_info sti)
  | ArrayType(id,te,ve) -> ArrayType(id,te, r_val_exp ve)

and r_node_info ni = {
  ni with
    static_params = List.map (flag r_static_param) ni.static_params;
    def = r_node_def ni.def;
}
and r_struct_type_info sti =
  Hashtbl.iter 
    (fun id fi -> Hashtbl.replace sti.st_ftable id (flag r_field_info fi)) 
    sti.st_ftable;
  sti

and r_field_info fi = { fi with fd_value = fopt r_val_exp fi.fd_value }

and r_node_def = function
  | Extern -> Extern
  | Abstract -> Abstract
  | Body(node_body) -> Body(r_node_body node_body)
  | Alias(by_pos_op) -> Alias(flag2 r_by_pos_op by_pos_op)

and r_packbody pb = 
  Hashtbl.iter 
    (fun id i -> Hashtbl.replace pb.pk_const_table id (flag r_const_info i))
    pb.pk_const_table;
  Hashtbl.iter
    (fun id i -> Hashtbl.replace pb.pk_type_table id (flag r_type_info i)) 
    pb.pk_type_table;
  Hashtbl.iter 
    (fun id i -> Hashtbl.replace pb.pk_node_table id (flag r_node_info i)) 
    pb.pk_node_table;
  pb

and r_node_body nb = {
  asserts = List.map (flag r_val_exp) nb.asserts;
  eqs     = List.map (flag r_eq_info) nb.eqs;
}
and r_eq_info (lpl,ve) = (List.map r_left_part lpl, r_val_exp ve)

and r_left_part = function 
  | LeftVar(id) -> LeftVar(id)
  | LeftField(lp,id) -> LeftField(r_left_part lp,id)
  | LeftArray(lp,ve) -> LeftArray(r_left_part lp, flag r_val_exp ve)  
  | LeftSlice(lp,si) -> LeftSlice(r_left_part lp, flag r_slice_info si)
