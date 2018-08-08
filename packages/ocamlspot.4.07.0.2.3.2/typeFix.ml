(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Utils

let ident id = Ident.create_persistent (Ident.name id)
  
let rec path = function
  | Path.Pident id -> Path.Pident (ident id)
  | Path.Pdot (t, name, pos) -> Path.Pdot (path t, Name.create name pos, pos)
  | Path.Papply (t1, t2) -> Path.Papply (path t1, path t2)
        
module TypeTable = Hashtbl.Make(Types.TypeOps)
open Types
open Btype

let type_expr =
  let cache = TypeTable.create 107 in
  let rec f t =
    let t = repr t in
    try TypeTable.find cache t with Not_found ->
      (* We need to create the cache entry first *)
      let t_dest = { t with desc = Tnil (* to be replaced *) } in
      TypeTable.add cache t t_dest;
      let default t =
        let desc = Btype.copy_type_desc f t.desc in
        match desc with
        | Tconstr (p, args, abbrev_memo_ref) ->
            (* abbrev_memo_ref is ignored *)
            Tconstr (path p, args, abbrev_memo_ref)
        | Tobject (_, { contents = None } ) -> desc
        | Tobject (t, { contents = Some (p, ts) }) ->
            Tobject (t, ref (Some (path p, ts)))
        | Tpackage (p, strs, tys) -> Tpackage (path p, strs, tys) (* CR: strs should be renamed too ? *)
        | Tvar _ | Tarrow _ | Ttuple _ | Tfield _ | Tnil 
        | Tsubst _ | Tvariant _ 
        | Tlink _ | Tpoly _ | Tunivar _ -> desc
      in
      (* Exception: printer of optional argument is bit special.
         We cannot rename the option type *)
      let desc = 
	  match t.desc with
	  | Tarrow(l, ty1, ty2, comm) when Btype.is_optional l ->
            begin match (repr ty1).desc with
            | Tconstr(path, [ty], abbrev) when Path.same path Predef.path_option ->
                (* we do not copy abbrev but it is ok *)
      	        Tarrow (l, 
	  		{ ty1 with desc = Tconstr(path, [f ty], abbrev) },
	  		f ty2,
	  		comm)
            | _ -> (* not option ? *) default t
            end
	  | Tvariant row_desc -> (* we cannot use copy_type_desc *)
            (* not sure ... *)
	      Tvariant (Btype.copy_row f true row_desc true 
			  (f row_desc.row_more))
	  | _ -> default t
      in
      t_dest.desc <- desc;
      t_dest
  in
  f
;;

let value_description vdesc = 
  { vdesc with val_type = type_expr vdesc.val_type }

let type_declaration tdecl = 
  { tdecl with type_params = List.map type_expr tdecl.type_params;
    type_manifest = Option.map ~f:type_expr tdecl.type_manifest }

let constructor_arguments = function
  | Cstr_tuple tys -> Cstr_tuple (List.map type_expr tys)
  | Cstr_record _lds -> assert false (* CR jfuruse: not yet *)

let exception_declaration edecl = 
  { edecl with ext_args = constructor_arguments edecl.ext_args }

let rec class_type = function
  | Cty_constr (p, tys, clty) ->
      Cty_constr (path p, List.map type_expr tys, class_type clty)
  | Cty_signature clsig -> Cty_signature (class_signature clsig)
  | Cty_arrow (l, ty, clty) -> Cty_arrow (l, type_expr ty, class_type clty)

and class_signature clsig = 
  { clsig with csig_self = type_expr clsig.csig_self;
    csig_vars = 
      Vars.map (fun (f1,f2,ty) -> (f1,f2, type_expr ty)) clsig.csig_vars;
    csig_inher = 
      List.map (fun (p, tys) -> path p, List.map type_expr tys)
        clsig.csig_inher }

let class_declaration cldecl = 
  { cldecl with cty_params = List.map type_expr cldecl.cty_params;
    cty_type = class_type cldecl.cty_type;
    cty_path = path cldecl.cty_path;
    cty_new = Option.map cldecl.cty_new ~f:type_expr }

let class_type_declaration ctd =
  { ctd with clty_params = List.map type_expr ctd.clty_params;
    clty_type = class_type ctd.clty_type;
    clty_path = path ctd.clty_path }

let rec module_type = function
  | Mty_ident p -> Mty_ident (path p)
  | Mty_signature sg -> Mty_signature (signature sg)
  | Mty_functor (id, mty, mty') ->
      Mty_functor (ident id, Option.map ~f:module_type mty, module_type mty')
  | Mty_alias (alias_presence, p) -> Mty_alias (alias_presence, path p)

and signature sg = List.map signature_item sg

and signature_item = function
  | Sig_value (id, vdesc) -> Sig_value (ident id, value_description vdesc)
  | Sig_type (id, tdecl, rec_status) -> 
      Sig_type (ident id, type_declaration tdecl, rec_status)
  | Sig_typext (id, edecl, ext_status) -> 
      Sig_typext (ident id, exception_declaration edecl, ext_status)
  | Sig_module (id, mod_decl, rec_status) ->
      Sig_module (ident id, module_declaration mod_decl, rec_status)
  | Sig_modtype (id, mty_decl) -> 
      Sig_modtype (ident id, modtype_declaration mty_decl)
  | Sig_class (id, cldecl, rec_status) ->
      Sig_class (ident id, class_declaration cldecl, rec_status)
  | Sig_class_type (id, cltdecl, rec_status) ->
      Sig_class_type (ident id, class_type_declaration cltdecl, rec_status)

and module_declaration md =
  { md with 
    md_type = module_type md.md_type;
  }

and modtype_declaration mtd = 
  { mtd with
    mtd_type = Option.map ~f:module_type mtd.mtd_type
  }
