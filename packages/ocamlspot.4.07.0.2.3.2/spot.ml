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
open Ext
open Format

let magic_number = "OCamlSpot"
let ocaml_version = "4.04.0"
let version = "2.3.2"

(** Kind of ``object`` *)
module Kind = struct
  type t =
    | Value | Type | Exception
    | Module | Module_type
    | Class | Class_type
    | Constructor | Field

  let to_string = function
    | Value       -> "v"
    | Type        -> "t"
    | Exception   -> "e"
    | Module      -> "m"
    | Module_type -> "mt"
    | Class       -> "c"
    | Class_type  -> "ct"
    | Constructor -> "constr"
    | Field       -> "field"

  (* for messages *)
  let name = function
    | Value       -> "value"
    | Type        -> "type"
    | Exception   -> "exception"
    | Module      -> "module"
    | Module_type -> "module_type"
    | Class       -> "class"
    | Class_type  -> "class_type"
    | Constructor -> "constructor"
    | Field       -> "field"

  (* used for query interface *)
  let from_string = function
    | "v"  | "value"           -> Value
    | "t"  | "type"            -> Type
    | "e"  | "exception"       -> Exception
    | "m"  | "module"          -> Module
    | "mt" | "module_type"     -> Module_type
    | "c"  | "class"           -> Class
    | "ct" | "class_type"      -> Class_type
    | "constr" | "constructor" -> Constructor
    | "field"                  -> Field
    | _ -> raise Not_found
end

(** module definition abstraction *)
module Abstraction = struct

  (* Types may be incompatible between compiler versions *)
  type module_expr =
    | AMod_ident      of Path.t (* module M = N *)
    | AMod_packed     of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | AMod_structure  of structure (* module M = struct ... end *)
    | AMod_functor    of Ident.t * Types.module_type option * module_expr (* module M(I:S) = *)
    | AMod_apply      of module_expr * module_expr (* module M = N(O) *)
    | AMod_constraint of module_expr * Types.module_type
    | AMod_unpack     of module_expr
    | AMod_abstract (* used for Tmodtype_abstract *)
    | AMod_functor_parameter

  and structure = structure_item list

  and structure_item =
    | AStr_value       of Ident.t
    | AStr_type        of Ident.t * structure
    | AStr_exception   of Ident.t
    | AStr_module      of Ident.t * module_expr option
    | AStr_modtype     of Ident.t * module_expr option
    | AStr_class       of Ident.t
    | AStr_class_type  of Ident.t
    | AStr_included    of Ident.t * module_expr * Kind.t * Ident.t
    | AStr_constructor of Ident.t
    | AStr_field       of Ident.t

  let rec format_module_expr ppf = function
    | AMod_ident p       -> fprintf ppf "%s" (Path.name p)
    | AMod_packed s      -> fprintf ppf "packed(%s)" s
    | AMod_structure str -> format_structure ppf str
    | AMod_functor (id, mtyopt, mexp) ->
        fprintf ppf "@[<4>\\(%s : %a) ->@ %a@]"
	  (Ident.name id)
          (fun ppf -> function
            | None -> fprintf ppf "<none>"
            | Some mty -> Printtyp.modtype ~with_pos:true ppf mty) mtyopt
          format_module_expr mexp
    | AMod_apply (mexp1, mexp2) ->
        fprintf ppf "%a(%a)"
          format_module_expr mexp1
          format_module_expr mexp2
    | AMod_constraint (mexp, mty) ->
        fprintf ppf "@[%a@ :@ @[%a@]@]"
          format_module_expr mexp
          (Printtyp.modtype ~with_pos:true) mty
    | AMod_abstract          -> fprintf ppf "<abst>"
    | AMod_functor_parameter -> fprintf ppf "<functor_parameter>"
    | AMod_unpack mty ->
        fprintf ppf "@[unpack@ : @[%a@]@]"
          format_module_expr mty

  and format_structure ppf items =
    fprintf ppf "{ @[<v>%a@] }"
      (list ";@," format_structure_item) items

  and format_structure_item ppf = function
    | AStr_value id     -> fprintf ppf "val %s" (Ident.name id)
    | AStr_constructor id -> fprintf ppf "constructor %s" (Ident.name id)
    | AStr_field id     -> fprintf ppf "field %s" (Ident.name id)
    | AStr_type (id, td) -> fprintf ppf "type %s @[%a@]" (Ident.name id) format_structure td
    | AStr_exception id -> fprintf ppf "exception %s" (Ident.name id)
    | AStr_module (id, mexp) ->
        fprintf ppf "@[<v4>module %s =@ %a@]"
          (Ident.name id)
          (format_option format_module_expr) mexp
    | AStr_modtype (id, mexp) ->
        fprintf ppf "@[<v4>module type %s =@ %a@]"
          (Ident.name id)
          (format_option format_module_expr) mexp
    | AStr_class id      -> fprintf ppf "class %s" (Ident.name id)
    | AStr_class_type id -> fprintf ppf "class type %s" (Ident.name id)
    | AStr_included (id, mexp, kind, id') ->
        fprintf ppf "@[<v4>included %s %a = %a@ { @[<v>%a@] }@]"
          (Kind.name kind)
          Ident.format id
          Ident.format id'
          format_module_expr mexp

  and format_option f ppf = function
    | None -> fprintf ppf "<none>"
    | Some v -> fprintf ppf "%a" f v

  let ident_of_structure_item : structure_item -> (Kind.t * Ident.t) = function
    | AStr_value id                  -> (Kind.Value, id)
    | AStr_type (id, _)              -> (Kind.Type, id)
    | AStr_exception id              -> (Kind.Exception, id)
    | AStr_module (id, _)            -> (Kind.Module, id)
    | AStr_modtype (id, _)           -> (Kind.Module_type, id)
    | AStr_class id                  -> (Kind.Class, id)
    | AStr_class_type id             -> (Kind.Class_type, id)
    | AStr_included (id, _, kind, _) -> (kind, id)
    | AStr_constructor id            -> (Constructor, id)
    | AStr_field id                  -> (Field, id)

  module Module_expr = struct
    (* cache key is Typedtree.module_expr *)
    module M = struct
      type t = Typedtree.module_expr
      let equal m1 m2 = m1 == m2
      let hash_source m = m.Typedtree.mod_loc
      let hash m = Hashtbl.hash (hash_source m)
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  module Structure_item = struct
    (* cache key is Abstraction.structure_item, not Typedtree.structure_item *)
    module M = struct
      type t = structure_item
      let equal s1 s2 =
        match s1, s2 with
        | AStr_value id1, AStr_value id2
        | AStr_exception id1, AStr_exception id2
        | AStr_class id1, AStr_class id2
        | AStr_class_type id1, AStr_class_type id2 -> id1 = id2
        | AStr_module (id1, mexp1) , AStr_module (id2, mexp2) ->
            id1 = id2 && Module_expr.equal mexp1 mexp2
        | AStr_modtype (id1, mty1), AStr_modtype (id2, mty2) ->
            id1 = id2 && Module_expr.equal mty1 mty2
        | AStr_included (id1, mexp1, kind1, id1'), AStr_included (id2, mexp2, kind2, id2') ->
            id1 = id2 && kind1 = kind2 && id1' = id2'
            && Module_expr.equal mexp1 mexp2
        | AStr_type (id1, td1), AStr_type (id2, td2) ->
            id1 = id2 && td1 = td2
        | AStr_constructor id1, AStr_constructor id2 -> id1 = id2
        | AStr_field id1, AStr_field id2 -> id1 = id2
        | (AStr_value _ | AStr_type _ | AStr_exception _ | AStr_modtype _
	   | AStr_class _ | AStr_class_type _ | AStr_module _ | AStr_included _
           | AStr_constructor _ | AStr_field _),
	  (AStr_value _ | AStr_type _ | AStr_exception _ | AStr_modtype _
	   | AStr_class _ | AStr_class_type _ | AStr_module _ | AStr_included _
           | AStr_constructor _ | AStr_field _) -> false

      let hash = Hashtbl.hash
    end
    include M
    module Table = Hashtbl.Make(M)
  end
end

module Annot = struct
  type t =
    | Use               of Kind.t * Path.t
    | Type              of Types.type_expr * Env.t * [`Expr of Path.t option | `Pattern of Ident.t option ]
    | Mod_type          of Types.module_type
    | Str_item          of Abstraction.structure_item
    | Module            of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive     of bool (* CR jfuruse: not used *)

  let _equal t1 t2 = match t1, t2 with
    | Type (t1, _, _), Type (t2, _, _) -> t1 == t2
    | Mod_type mty1, Mod_type mty2 -> mty1 == mty2
    | Str_item sitem1, Str_item sitem2 -> Abstraction.Structure_item.equal sitem1 sitem2
    | Module mexp1, Module mexp2 -> mexp1 == mexp2
    | Use (k1,p1), Use (k2,p2) -> k1 = k2 && p1 = p2
    | Non_expansive b1, Non_expansive b2 -> b1 = b2
    | Functor_parameter id1, Functor_parameter id2 -> id1 = id2
    | (Type _ | Str_item _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _
          | Mod_type _),
      (Type _ | Str_item _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _
          | Mod_type _) -> false

  let string_of_at = function
    | `Expr _    -> "Expr"
    | `Pattern _ -> "Pattern"

  let format ppf = function
    | Type (typ, _env, at) ->
	Printtyp.reset ();
	Printtyp.mark_loops typ;
        (* CR jfuruse: not fancy having @. *)
	fprintf ppf "Type: %a@ " (Printtyp.type_scheme ~with_pos:false) typ;
	fprintf ppf "XType: %a@ " (Printtyp.type_scheme ~with_pos:true) typ;
        fprintf ppf "At: %s" (string_of_at at)
    | Mod_type mty ->
	fprintf ppf "Type: %a@ " (Printtyp.modtype ~with_pos:false) mty;
	fprintf ppf "XType: %a" (Printtyp.modtype ~with_pos:true) mty
    | Str_item str ->
	fprintf ppf "Str_item: %a"
	  Abstraction.format_structure_item str
    | Use (use, path) ->
	fprintf ppf "Use: %s, %s"
	  (String.capitalize_ascii (Kind.name use)) (Path.name path)
    | Module mexp ->
	fprintf ppf "Module: %a"
          Abstraction.format_module_expr mexp
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b

  let summary ppf = function
    | Type (_typ, _env, at) ->
        (* CR jfuruse: not fancy having @. *)
        fprintf ppf "Type: ...@ ";
        fprintf ppf "XType: ...@ ";
        fprintf ppf "At: %s" (string_of_at at)
    | Mod_type _mty ->
        fprintf ppf "Type: ...@ ";
        fprintf ppf "XType: ..."
    | Str_item _str ->
        fprintf ppf "Str_item: ..."
    | Use (use, path) ->
        fprintf ppf "Use: %s, %s"
          (String.capitalize_ascii (Kind.name use)) (Path.name path)
    | Module _mexp ->
	fprintf ppf "Module: ..."
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b

  let dummy = Use (Kind.Value, Path.Pident (Ident.create_persistent "dummy"))
end

module EXTRACT = struct
  open Types
  open! Typedtree
  open Asttypes
  open Abstraction
  open Annot

  let cache_module_expr = Module_expr.Table.create 31
  let cache_structure_item = Structure_item.Table.create 31

  let clear_cache () =
    Module_expr.Table.clear cache_module_expr;
    Structure_item.Table.clear cache_structure_item

  let tbl = Hashtbl.create 1023 (* CR jfuruse: global *)

  type location_property = Wellformed | Flipped | Over_files | Illformed

  let check_location loc =
    let open Location in
    let open Lexing in
    if loc.loc_start == dummy_pos || loc.loc_end == dummy_pos then Illformed
    else if loc.loc_start = dummy_pos || loc.loc_end = dummy_pos then Illformed
    else
      (* If the file name is different between the start and the end, we cannot tell the wellformedness. *)
      if loc.loc_start.pos_fname <> loc.loc_end.pos_fname then Over_files
      else
        (* P4 creates some flipped locations where loc_start > loc_end *)
        match compare loc.loc_start.pos_cnum loc.loc_end.pos_cnum
        with
        | -1 | 0 -> Wellformed
        | _ -> Flipped

  let record loc t =
    let really_record () =
      let records = try Hashtbl.find tbl loc with Not_found -> [] in
(*
        (* CR jfuruse: I am not really sure the below is correct now,
           but I remember the huge compilation slow down... *)
        (* This caching works horribly when too many things are defined
           at the same location. For example, a type definition of more than
           3000 variants, with sexp camlp4 extension, the compile time explodes
           from 10secs to 4mins! Therefore this works
           only if [num_records <= 10]
        *)
        if num_records <= 10 && List.exists (equal t) records then ()
        else Hashtbl.replace tbl loc (num_records + 1, t :: records)
*)
      Hashtbl.replace tbl loc (t :: records)
    in
    match check_location loc with
    | Wellformed -> really_record ()
    | Flipped | Illformed ->
        if not loc.loc_ghost then Format.eprintf "%aWarning: Ill-formed location.@." Location.print loc
    | Over_files -> ()

  let record_def loc sitem = record loc (Str_item sitem)
  let record_use loc kind path = record loc (Use (kind, path))

  let record_use_construct loc kind path name = 
    (* Note, this is different from record_record and record_construct *)
    assert (match kind with Kind.Constructor | Field -> true | _ -> false);
    record loc (Use (kind, Path.Pdot (path, name, -1 (* dummy *)))) 

  let with_record_def loc sitem = record loc (Str_item sitem); sitem

  module T = struct

    let rec signature sg = AMod_structure (List.flatten (List.map signature_item sg))

    and signature_item = function
      | Sig_value (id, _)          -> [AStr_value id]
      | Sig_typext (id, _, _)      -> [AStr_exception id]
      | Sig_type (id, td, _)       -> [type_declaration id td]
      | Sig_module (id, md, _)    -> [AStr_module (id, Some (module_declaration md))]
      | Sig_modtype (id, mty_decl) -> [AStr_modtype (id, modtype_declaration mty_decl)]
      | Sig_class (id, _, _)       -> 
          (* CR jfuruse: Need to check what happens in includsion of class *)
          [AStr_class id; AStr_class_type id;  AStr_type (id, [])]
      | Sig_class_type (id, _, _)  -> [ AStr_class_type id ]

    and type_declaration id td = match td.type_kind with
      | Type_abstract -> AStr_type (id, [])
      | Type_variant lst -> 
          AStr_type (id, List.map (fun { Types.cd_id= id } -> AStr_constructor id) lst)
      | Type_record (lst, _) -> 
          AStr_type (id, List.map (fun { Types.ld_id = id } -> AStr_field id) lst)
      | Type_open -> AStr_type (id, []) (* CR jfuruse: Need to check *)
      
    and module_type = function
      | Mty_ident p -> AMod_ident p
      | Mty_signature sg -> signature sg
      | Mty_functor (id, mty1, mty2) -> AMod_functor(id, mty1, module_type mty2)
      | Mty_alias (_alias_presence, p) -> AMod_ident p (* CR jfuruse: need to check *)

    and module_declaration md = module_type md.md_type
      
    and modtype_declaration mtd = Option.map ~f:module_type mtd.mtd_type
  end

  let aliases_of_include' sg (* <= includee *) sg' (* <= includer *) =
    let sgstr = List.flatten (List.map T.signature_item sg) in
    let sgkidents = List.map ident_of_structure_item sgstr in
    let sg'str = List.flatten (List.map T.signature_item sg') in
    let sg'kidents = List.map ident_of_structure_item sg'str in

    List.map2 (fun (k,id) (k',id') ->
      assert (k=k');
      id', k, id (* id' is an alias of id of kind k *)
      ) sgkidents sg'kidents

  let aliases_of_include mexp includer_sg =
    let env' = try Cmt.recover_env mexp.mod_env with e -> 
      Format.eprintf "recover_env: %s@." (Printexc.to_string e);
      assert false 
    in 
    let sg = try match Mtype.scrape env' mexp.mod_type with 
      | Mty_signature sg -> sg 
      | _ -> prerr_endline "strange!";assert false 
      with _ -> assert false 
    in
    aliases_of_include' sg includer_sg

  let class_infos f { ci_virt=_;
                      ci_params=_ (* CR jfuruse: ? *); (* string loc list * Location.t; *)
                      ci_id_name = {loc}; (* : string loc; *)
                      ci_id_class; (* : Ident.t; *)
                      ci_id_class_type; (*  : Ident.t; *)
                      ci_id_object; (*  : Ident.t; *)
                      ci_id_typehash; (*  : Ident.t; *)
                      ci_expr; (* : 'a; *)
                      ci_decl=_; (* : Types.class_declaration; *)
                      ci_type_decl=_; (*  : Types.class_type_declaration; *)
                      ci_loc=_; (* : Location.t *) } =
      f ci_expr;
      List.map (with_record_def loc)
        [ AStr_class ci_id_class;
          AStr_class_type ci_id_class_type;
          AStr_type (ci_id_object, []);
          AStr_type (ci_id_typehash, []) ]

  let get_constr_path typ = 
    match (Ctype.repr typ).desc with
    | Tconstr (path, _, _) -> path
    | _ -> (* strange.. *) assert false

  let rec module_expr mexp =
    match Module_expr.Table.find cache_module_expr mexp with
    | None ->
        (* When a module definition finds itself in itself.
           Impossible to happen, so far. *)
        assert false
    | Some v -> v
    | exception Not_found ->
        record mexp.mod_loc (Mod_type mexp.mod_type);
        (* for recursive module *)
	Module_expr.Table.replace cache_module_expr mexp None;
	let res = module_expr_desc mexp.mod_desc in
	Module_expr.Table.replace cache_module_expr mexp (Some res);
        res

  and module_expr_desc = function
    | Tmod_ident (p, {loc}) -> 
        record_use loc Kind.Module p;
        AMod_ident p
    | Tmod_structure str ->
	(* This may recompute abstractions of structure_items.
	   It sounds inefficient but not so much actually, since
	   module_expr is nicely cached. *)
	structure str
    | Tmod_functor (id, {loc}, mtyo, mexp) ->
        ignore & Option.map ~f:module_type mtyo;
        record_def loc & AStr_module (id, Some AMod_functor_parameter);
	AMod_functor(id, Option.map mtyo ~f:(fun mty -> mty.mty_type), module_expr mexp)
    | Tmod_apply (mexp1, mexp2, _mcoercion) -> (* CR jfuruse ? *)
	AMod_apply (module_expr mexp1, module_expr mexp2)
    | Tmod_constraint (mexp, mty, cstraint, _mcoercion) ->
        module_type_constraint cstraint;
	AMod_constraint (module_expr mexp, mty)
    | Tmod_unpack (expr, mty) ->
        ignore & expression expr;
        AMod_unpack (T.module_type mty) (* CR jfuruse: need to unpack, really? *)

  and module_type_constraint = function
    | Tmodtype_implicit -> ()
    | Tmodtype_explicit mty -> ignore & module_type mty

  and structure str = AMod_structure (List.concat_map structure_item str.str_items)

  and structure_item sitem =
    (* it may recompute the same thing, but it is cheap *)
    let sitems = structure_item_desc sitem.str_loc sitem.str_desc in
    (* eq consing *)
    let equalize sitem =
      try
	Structure_item.Table.find cache_structure_item sitem
      with
      | Not_found ->
	  Structure_item.Table.replace cache_structure_item sitem sitem;
	  sitem
    in
    List.map equalize sitems

  and structure_item_desc loc0 = function
    | Tstr_eval (e, _) -> 
        ignore & expression e; 
        []
    | Tstr_value (_flag, vbs) ->
	List.concat_map (fun { vb_pat= pat; vb_expr= exp } ->
          expression exp;
          pattern pat) vbs
    | Tstr_primitive ({ val_id= id; val_name= {loc} } as vdesc) ->
        value_description vdesc;
        [ with_record_def loc & AStr_value id ]
    | Tstr_type (_rf, id_descs) -> 
        List.map (fun ({ typ_id= id; typ_name= {loc} } as td) -> 
          with_record_def loc & type_declaration id td) id_descs
    | Tstr_exception ec -> [ extension_constructor ec ]
    | Tstr_typext text -> type_extension text
    | Tstr_attribute _ -> []
    | Tstr_module { mb_id=id; mb_name= {loc}; mb_expr= mexp } ->
        record loc0 (Mod_type mexp.mod_type);
        [ with_record_def loc & AStr_module (id, Some (module_expr mexp)) ]
    | Tstr_recmodule mbs->
	List.map (fun { mb_id=id; mb_name= {loc}; mb_expr= mexp } ->
	  with_record_def loc & AStr_module (id, Some (module_expr mexp))) mbs
    | Tstr_modtype { mtd_id= id; mtd_name= {loc}; mtd_type= mtyo } -> 
        [ with_record_def loc & AStr_modtype (id, Option.map ~f:module_type mtyo) ]
    | Tstr_open { open_path= path; open_txt= {loc} } -> 
        record_use loc Kind.Module path;
        []
    | Tstr_class xs ->
	List.concat_map (fun (clsdecl, _names) -> 
          class_declaration clsdecl) xs
    | Tstr_class_type iddecls ->
	List.concat_map (fun (id, {loc}, clstydecl) -> 
          with_record_def loc (AStr_class_type id)
          :: class_type_declaration clstydecl) iddecls
    | Tstr_include { incl_mod=mexp; incl_type= sg } ->
        let idmap = try aliases_of_include mexp sg with e -> prerr_endline "structure_item include failed!!!"; raise e in
        let m = module_expr mexp in
        List.map (fun (id_includer, k, id_included) -> 
          with_record_def loc0 & AStr_included (id_includer, m, k, id_included)
        ) idmap

  and extension_constructor 
      { ext_id= id;
        ext_name= {loc};
        (* ext_type : Types.extension_constructor; *)
        ext_kind (* : extension_constructor_kind; *)
        (* ext_loc : Location.t; *)
        (* ext_attributes: attributes; *)
      } =
    begin match ext_kind with
    | Text_decl (cargs, ctyo) -> 
        constructor_arguments cargs;
        Option.iter ~f:core_type ctyo
    | Text_rebind (p, {loc}) ->  record_use loc Kind.Exception p
    end;
    with_record_def loc & AStr_exception id

  and constructor_arguments = function
    | Cstr_tuple ctys -> List.iter core_type ctys
    | Cstr_record _lds -> failwithf "record constructor argument is not supported yet"

  and type_extension { tyext_path = path;
                       tyext_txt = {loc};
                       tyext_constructors;
                     } =
    record_use loc Kind.Type path;
    List.map extension_constructor tyext_constructors

  (* CR jfuruse: TODO: caching like module_expr_sub *)
  and module_type mty = module_type_desc mty.mty_desc

  and module_type_desc = function
    | Tmty_ident (p, {loc}) -> 
        record_use loc Kind.Module_type p;
        AMod_ident p
    | Tmty_signature sg -> signature sg
    | Tmty_functor (id, {loc}, mty1o, mty2) ->
        (* CR jfuruse: need to scrape ? but how ? *)
        record_def loc & AStr_module (id, Option.map ~f:module_type mty1o);
        ignore & module_type mty2;
        AMod_functor(id, Option.map ~f:(fun x -> x.mty_type) mty1o, module_type mty2)
    | Tmty_with (mty, lst) -> 
        lst |> List.iter (fun (path, {loc}, with_constraint) ->
          record loc (Use ( (match with_constraint with
                             | Twith_type _      -> Kind.Type
                             | Twith_module _    -> Kind.Module
                             | Twith_typesubst _ -> Kind.Type
                             | Twith_modsubst _  -> Kind.Module)
                          , path)));
        module_type mty (* CR jfuruse: ?? *)
    | Tmty_typeof mexp -> module_expr mexp
    | Tmty_alias (p, {loc}) -> 
        record_use loc Kind.Module_type p; 
        AMod_ident p

  and signature sg = AMod_structure (List.concat_map signature_item sg.sig_items)

  and signature_item sitem =
    match sitem.sig_desc with
    | Tsig_value ({ val_id= id; val_name= {loc}; val_val } as vdesc) -> 
        record loc & Type (val_val.val_type, sitem.sig_env, `Pattern (Some id));
        value_description vdesc;
        [ with_record_def loc & AStr_value id ]
    | Tsig_type (_rf, typs) -> 
        List.map (fun ({ typ_id=id; typ_name= {loc} } as td) -> 
          with_record_def loc & type_declaration id td) typs
    | Tsig_exception ec -> [ extension_constructor ec ]
    | Tsig_module { md_id= id; md_name= {loc}; md_type= mty } ->
        record loc & Mod_type mty.mty_type;
        [ with_record_def loc & AStr_module (id, Some (module_type mty)) ]
    | Tsig_recmodule lst ->
        List.map (fun { md_id= id; md_name= {loc}; md_type= mty } -> 
          with_record_def loc & AStr_module (id, Some (module_type mty))) lst
    | Tsig_modtype { mtd_id=id; mtd_name= {loc}; mtd_type= mtyo } ->
        [ with_record_def loc & (* todo *) AStr_modtype (id, Option.map ~f:module_type mtyo) ]
        (* sitem.sig_final_env can be used? *)
    | Tsig_open { open_path= p; open_txt = {loc} } -> 
        record_use loc Kind.Module p;
        []
    | Tsig_include { incl_mod= mty; incl_type= sg } ->
        let m = module_type mty in
        let sg0 = try match Mtype.scrape (Cmt.recover_env mty.mty_env) mty.mty_type with Mty_signature sg -> sg | _ -> assert false with _ -> assert false in
        let idmap = try aliases_of_include' sg0 sg with _ -> assert false in
        List.map (fun (id, k, id') -> 
          with_record_def sitem.sig_loc & AStr_included (id, m, k, id')) idmap
    | Tsig_class clsdescrs ->
        List.concat_map class_description clsdescrs
    | Tsig_class_type clstydecls -> 
        List.concat_map class_type_declaration clstydecls
          (* AStr_class_type cls.ci_id_class)  *)
    | Tsig_typext text -> type_extension text
    | Tsig_attribute _ -> []

  and class_declaration      cd = class_infos class_expr cd

  and class_description      cd = class_infos class_type cd

  and class_type_declaration cd = class_infos class_type cd

  and class_expr ce = match ce.cl_desc with
    | Tcl_ident (p, {loc}, core_types) ->
        record_use loc Kind.Class p;
        List.iter core_type core_types
    | Tcl_structure cs -> class_structure cs
    | Tcl_fun (_label, pat, classvals, clexpr, _partial) ->
        ignore & pattern pat;
        class_values classvals;
        class_expr clexpr
    | Tcl_apply (clexpr, args) ->
        class_expr clexpr;
        List.iter (fun (_label, expropt) ->
          match expropt with
          | None -> ()
          | Some expr -> expression expr) args
    | Tcl_let (_rec_flag, vbs, classvals, clexpr) ->
        class_values classvals;
        List.iter (fun { vb_pat=pat; vb_expr= expr} ->
          ignore & pattern pat;
          expression expr) vbs;
        class_expr clexpr
    | Tcl_constraint (clexpr, cltypeopt, _names, _names2, _concr) ->
        class_expr clexpr;
        begin match cltypeopt with
        | Some cltyp -> class_type cltyp
        | None -> ()
        end
    | Tcl_open (_, p, lid, _env, clexp) ->
        record_use lid.loc Kind.Module p;
        class_expr clexp

  and class_values xs =
    (* I guess it is an info of class creation variables as class members *)
    List.iter (fun (id, {loc}, expr) ->
      record_def loc & AStr_value id;
      expression expr) xs
      
  and class_type cltyp = match cltyp.cltyp_desc with
    | Tcty_constr (p, {loc}, core_types) ->
        record_use loc Kind.Class_type p;
        List.iter core_type core_types
    | Tcty_signature clsig ->
        class_signature clsig
    | Tcty_arrow (_label, ctype, cltype) ->
        core_type ctype;
        class_type cltype
    | Tcty_open (_, p, lid, _env, clty) ->
        record_use lid.loc Kind.Module p;
        class_type clty

  and class_signature { csig_self;
                        csig_fields;
                        csig_type=_;
                      } =
    core_type csig_self;
    List.iter class_type_field csig_fields

  and class_type_field { ctf_desc; ctf_loc=_ } = match ctf_desc with
    | Tctf_inherit cltyp -> class_type cltyp
    | Tctf_val (_name, _mutable_flag, _virtual_flag, ctype) ->
        core_type ctype
    | Tctf_method (_name, _private_flag, _virtual_flag, ctype) ->
        core_type ctype
    | Tctf_constraint (ctype1, ctype2) -> 
        core_type ctype1;
        core_type ctype2
    | Tctf_attribute _ -> ()

  and class_structure
      { cstr_self; (* : pattern; *)
        cstr_fields; (* : class_field list; *)
        cstr_type=_;
        cstr_meths=_; (* ? *) (* : Ident.t Meths.t *) } =
    ignore & pattern cstr_self;
    List.iter class_field cstr_fields

  and class_field 
        { cf_desc; (*  : class_field_desc; *)
          cf_loc=_ } = 
        match cf_desc with
        | Tcf_inherit (_override_flag, clexpr, _nameopt (* ? *), inh_vars, inh_meths) -> 
            let loc = clexpr.cl_loc in
            (* CR jfuruse: We should to have a way to seek the inherited var 
               into the super class... *)
            List.iter (fun (_, id) -> record_def loc & AStr_value id) inh_vars;
            (* CR jfuruse: meths should be spotted ... *)
            List.iter (fun (_, id) -> record_def loc & AStr_value id) inh_meths;
            class_expr clexpr
        | Tcf_val ({loc}, _mutable_flag, id, clfieldk, _bool) -> 
            record_def loc & AStr_value id;
            class_field_kind clfieldk
        | Tcf_method ({loc=_loc}, _private_flag, clfieldk) ->
            class_field_kind clfieldk
        | Tcf_constraint (cty1, cty2) ->
            core_type cty1; 
            core_type cty2
        | Tcf_initializer expr -> expression expr
        | Tcf_attribute _ -> ()

  and class_field_kind = function
    | Tcfk_virtual cty -> core_type cty
    | Tcfk_concrete (_override_flag, expr) -> expression expr

  and type_declaration id 
      { typ_params=_; (* CR jfuruse ? : string loc option list; *)
        typ_type=_; (* : Types.type_declaration; *)
        typ_cstrs=_; (* CR jfuruse? : (core_type * core_type * Location.t) list; *)
        typ_kind; (* : type_kind; *)
        typ_private=_; (* : private_flag; *)
        typ_manifest; (* : core_type option; *)
        typ_loc=_; } =
    Option.iter ~f:core_type typ_manifest; 
    match typ_kind with
    | Ttype_abstract -> AStr_type (id, [])
    | Ttype_variant lst -> 
        AStr_type (id, List.map (fun { cd_id=id; cd_name= {loc}; cd_args } ->
          constructor_arguments cd_args;
          with_record_def loc & AStr_constructor id) lst)
    | Ttype_record lst -> 
        AStr_type (id, List.map (fun { ld_id=id; ld_name= {loc}; ld_type= cty } -> 
          core_type cty;
          with_record_def loc & AStr_field id) lst)
    | Ttype_open -> AStr_type (id, [])

  and value_binding_list xs = 
    xs |> List.iter (fun { vb_pat=pat; vb_expr= expr } -> 
      ignore & pattern pat;
      expression expr)

  and case_list cases = 
    cases |> List.iter (fun case ->
      ignore & pattern case.c_lhs;
      ignore & Option.map ~f:expression case.c_guard;
      expression case.c_rhs)

  and label_description loc p {lbl_name } =
    record_use_construct loc Kind.Field p lbl_name

  and expression 
      { exp_desc; (* : expression_desc; *)
        exp_loc=loc0;
        exp_extra=eextras; (*  : (exp_extra * Location.t * attributes) list; *)
        exp_type; (* : type_expr; *)
        exp_env; (* : Env.t *) } =
    let popt = match exp_desc with
      | Texp_ident (p, _loc, _) -> Some p
      | _ -> None
    in
    record loc0 (Type (exp_type, exp_env, `Expr popt)); (* `Expr is required? *)
    List.iter (fun (eextra, _loc, _) -> exp_extra eextra) eextras;
    match exp_desc with
    | Texp_ident (p, {loc}, _) -> 
        record_use loc Kind.Value p
    | Texp_constant _constant -> ()
    | Texp_let (_rec_flag, vbs, expr) -> 
        value_binding_list vbs;
        expression expr
    | Texp_function { arg_label=_; param=_; cases; partial=_ } -> 
        case_list cases
    | Texp_apply (expr, leos) ->
        expression expr;
        leos |> List.iter (fun (_label, expropt) ->
          match expropt with
          | None -> ()
          | Some expr -> expression expr)
    | Texp_match (expr, cases, cases', _(*partial*)) ->
        expression expr;
        case_list cases;
        case_list cases'
    | Texp_try (expr, cases) ->
        expression expr;
        case_list cases
    | Texp_tuple exprs ->
        List.iter expression exprs
    | Texp_construct ({loc=_loc}, cdesc, exprs) -> 
        begin match cdesc.Types.cstr_tag with
        | Types.Cstr_extension (path, _) ->
            record loc0 (* whole (Failure "xxx") *) (Use (Kind.Exception, path))
        | _ ->
            let path = get_constr_path cdesc.Types.cstr_res in
            record_use_construct loc0 Kind.Constructor path cdesc.Types.cstr_name
        end;
        List.iter expression exprs
    | Texp_variant (_name, None) -> ()
    | Texp_variant (_name, Some e) -> expression e
    | Texp_record { fields; extended_expression= expropt } ->
        let p = get_constr_path exp_type in
        record loc0 (Use (Kind.Type, p));
        Option.iter ~f:expression expropt;
        fields |> Array.iter (fun (ld, rld) ->
          match rld with
          | Kept _ -> ()
          | Overridden ({loc},e) ->
              label_description loc p ld;
              expression e)

    | Texp_field (expr, {loc}, ldesc) ->
        expression expr;
        let p = get_constr_path expr.exp_type in
        label_description loc p ldesc
    | Texp_setfield (expr, {loc}, ldesc, expr') ->
        expression expr;
        expression expr';
        let p = get_constr_path expr.exp_type in
        label_description loc p ldesc
    | Texp_array es -> List.iter expression es
    | Texp_ifthenelse (e1, e2, eopt) -> 
        expression e1;
        expression e2;
        Option.iter ~f:expression eopt
    | Texp_sequence (e1, e2)
    | Texp_while (e1, e2) ->
        expression e1;
        expression e2
    | Texp_for (id, { ppat_loc= loc }, e1, e2, _direction_flag, e3) ->
        record loc (Type (Predef.type_int, Env.initial_safe_string (* unsafe is also ok *), `Pattern (Some id)));
        record_def loc (AStr_value id);
        List.iter expression [e1; e2; e3]
    | Texp_send (e, m, eopt) ->
        expression e;
        meth loc0 m; (* Wow meth can have ident! but it lacks location! *)
        Option.iter ~f:expression eopt
    | Texp_new (p, {loc}, _ (* Types.class_declaration *)) ->
        record_use loc Kind.Class p
    | Texp_instvar (_p1 (* class? *), p2, {loc}) ->
        record_use loc Kind.Value p2
    | Texp_setinstvar (_p1 (* class? *), p2, {loc}, expr) ->
        record_use loc Kind.Value p2;
        expression expr
    | Texp_override (_P1 (* class? *), bindings) ->
        bindings |> List.iter (fun (p, {loc}, expr) ->
          record_use loc Kind.Value p; (* is it a method? *)
          expression expr)
    | Texp_letmodule (id, {loc}, mexp, expr) ->
        record_def loc & AStr_module (id, Some (module_expr mexp));
        expression expr
    | Texp_assert e 
    | Texp_lazy e -> expression e
    | Texp_object (clstr, _names) -> class_structure clstr
    | Texp_pack mexp -> ignore & module_expr mexp
    | (Texp_unreachable|Texp_extension_constructor (_, _)) -> assert false (* CR jfuruse: Not yet *)
    | Texp_letexception (ext_con, e) ->
        ignore (extension_constructor ext_con); (* XXX correct? *)
        expression e

  and exp_extra = function
    | Texp_constraint cty -> core_type cty
    | Texp_coerce (ctyo, cty) -> 
        Option.iter ~f:core_type ctyo;
        core_type cty
    | Texp_open (_override_flag, path, {loc}, _env) ->
        record_use loc Kind.Module path
    | Texp_poly ctyo ->
        Option.iter ~f:core_type ctyo
    | Texp_newtype _string -> () (* CR jfuruse: todo *)

  and pattern 
      { pat_desc; (* : pattern_desc; *)
        pat_loc=loc0;
        pat_extra=pextras;  (*  : (pat_extra * Location.t * attributes) list; *)
        pat_type; (*: type_expr; *)
        pat_env } = 
    let idopt = match pat_desc with
      | Tpat_var (id, _) -> Some id
      | _ -> None
    in
    record loc0 (Type (pat_type, pat_env, `Pattern idopt)); (* `Expr is required? *)
    List.iter (fun (pextra, _loc, _) -> pat_extra pextra) pextras;
    match pat_desc with
    | Tpat_any -> []
    | Tpat_var (id, {loc}) -> 
        [ with_record_def loc & AStr_value id ]
    | Tpat_alias (pat, id, {loc}) ->
        with_record_def loc (AStr_value id) :: pattern pat
    | Tpat_constant _constant -> []
    | Tpat_tuple pats ->
        List.concat_map pattern pats
    | Tpat_construct ({loc=_loc}, cdesc, pats) ->
        begin match cdesc.Types.cstr_tag with
        | Types.Cstr_extension (path, _) ->
            record loc0 (* whole (Failure "xxx") *) (Use (Kind.Exception, path))
        | _ ->
            let path = get_constr_path cdesc.Types.cstr_res in
            record_use_construct loc0 Kind.Constructor path cdesc.Types.cstr_name
        end;
        List.concat_map pattern pats
    | Tpat_variant (_label, patopt, {contents = _row_desc}) ->
        (* I bleive row_desc can be ignored *)
        begin match patopt with
        | Some p -> pattern p
        | None -> []
        end
    | Tpat_record (fields, _closed_flag) ->
        let p = get_constr_path pat_type in
        record loc0 (Use (Kind.Type, p));
        List.concat_map (fun ({loc}, ldesc, pat) ->
          label_description loc p ldesc;
          pattern pat) fields
    | Tpat_array pats ->
        List.concat_map pattern pats
    | Tpat_or (p1, p2, _row_desc_opt) ->
        pattern p1 @ pattern p2
    | Tpat_lazy p -> pattern p

  and pat_extra = function
    | Tpat_constraint cty -> core_type cty
    | Tpat_type (p, {loc}) -> record_use loc Kind.Type p
    | Tpat_unpack -> ()
    | Tpat_open (p, {loc}, _env) -> record_use loc Kind.Module p

  and meth _loc = function
    | Tmeth_name _name -> ()
    | Tmeth_val _id -> 
        (* record_use loc ...id ... Oh, we cannot have the loc of this id. 
                          CR jfuruse: OCaml requires a fix
                       *)
        ()

  and value_description { Typedtree.val_desc } = core_type val_desc

  and core_type 
      { ctyp_desc;
        ctyp_type=_;
        ctyp_env=_;
        ctyp_loc=_; } = match ctyp_desc with
      | Ttyp_any 
      | Ttyp_var _ -> ()
      | Ttyp_arrow (_label, cty1, cty2) ->
          core_type cty1; core_type cty2
      | Ttyp_tuple ctys -> List.iter core_type ctys
      | Ttyp_constr (p, {loc}, ctys) -> 
          record_use loc Kind.Type p;
          List.iter core_type ctys
      | Ttyp_object (l_a_core_type_list, _close_flag) -> 
          List.iter (function
              | OTtag (_, _, cty) -> core_type cty
              | OTinherit cty -> core_type cty
            ) l_a_core_type_list
      | Ttyp_class (p, {loc}, ctys) ->
          record_use loc Kind.Class p;
          List.iter core_type ctys
      | Ttyp_alias (cty, _string (* ? *)) -> core_type cty
      | Ttyp_variant (row_fields, _bool, _labels) -> List.iter row_field row_fields
      | Ttyp_poly (_vars (* ? *), cty) -> core_type cty
      | Ttyp_package pty -> package_type pty

  and row_field = function
    | Ttag (_label, _atrs, _bool, ctys) -> List.iter core_type ctys
    | Tinherit cty -> core_type cty

  and package_type 
      { pack_path; (* : Path.t; *)
        pack_fields; (* : (Longident.t loc * core_type) list; *)
        pack_type=_; (* : Types.module_type; *)
        pack_txt= {loc} (*  : Longident.t loc; *) } =
    record_use loc Kind.Module pack_path;
    List.iter (fun (_lident_loc, cty) -> core_type cty) pack_fields

  let top_structure str = 
    clear_cache (); 
    Hashtbl.clear tbl;
    match structure str with
    | AMod_structure str -> str, tbl
    | _ -> assert false

  let top_signature sg =  
    clear_cache (); 
    Hashtbl.clear tbl;
    match signature sg with
    | AMod_structure str -> str, tbl
    | _ -> assert false
end

module Position = struct
  open Lexing

  type t = { line_column : (int * int) option;
             bytes : int option }

  let of_lexing_position pos =
    { line_column = Some (pos.pos_lnum, pos.pos_cnum - pos.pos_bol);
      bytes = Some pos.pos_cnum }

  let compare p1 p2 = match p1, p2 with
    (* line_columns are preferrable, since bytes of mll+mly are of the generated files *)
    | { line_column = Some (l1,c1); _ }, { line_column = Some (l2,c2); _ } ->
	begin match compare l1 l2 with
	| 0 -> compare c1 c2
	| n -> n
	end
    | { bytes = Some b1; _ }, { bytes = Some b2; _ } -> compare b1 b2
    | _ -> assert false

  let to_string p = match p.line_column, p.bytes with
    | Some (l,c), Some b -> Printf.sprintf "l%dc%db%d" l c b
    | Some (l,c), None -> Printf.sprintf "l%dc%d" l c
    | None, Some b -> Printf.sprintf "b%d" b
    | None, None -> assert false

  let none = { line_column = None; bytes = None }

  exception Parse_failure of string

  let parse s =
    (* token : [a-z][0-9]+ *)
    let len = String.length s in
    let rec get_number ~num pos =
      if pos >= len then num, pos
      else
	match s.[pos] with
	| '0'..'9' ->
	    get_number
	      ~num: (num * 10 + int_of_char s.[pos] - int_of_char '0')
	      (pos + 1)
	| _ -> num, pos
    in
    let rec get_tokens pos =
      if pos >= len then []
      else
	match s.[pos] with
	| 'a'..'z' ->
	    let k = s.[pos] in
	    let pos = pos + 1 in
	    let num, pos' = get_number ~num:0 pos in
	    if pos = pos' then
	      raise (Parse_failure (Printf.sprintf "pos token has no number: '%c'"
				       k));
	    (k, num) :: get_tokens pos'
        | '0'..'9' ->
            (* Good Ol' Syntax *)
            begin try ['b', int_of_string s] with _ ->
              raise (Parse_failure
                        (Printf.sprintf "failed to parse %S as a byte position" s))
            end
	| _ ->
	    raise (Parse_failure (Printf.sprintf "illegal pos token head '%c'"
				     s.[pos]))
    in
    let tokens = get_tokens 0 in
    match tokens with
    | ['l', line; 'c', column] -> { line_column = Some (line, column);
				    bytes = None }
    | ['b', bytes] -> { line_column = None; bytes = Some bytes }
    | _ -> raise (Parse_failure "illegal pos token combination")

  let next = function
    | { bytes = Some b; _ } -> { bytes = Some (b + 1); line_column = None }
    | { line_column = Some (l,c); bytes = None; } ->
        { line_column = Some (l, c+1); bytes = None }
    | _ -> assert false

  let is_complete = function
    | { line_column = Some _ } -> true
    | _ -> false

  (* it drops one byte at the end, but who cares? *)
  let complete mlpath t = match t with
    | { line_column = Some _ } ->
        t (* already complete *)
    (* Completing of the byte part from line-column is HARD,
       for the case of auto-generated source files.
       line_column : this is of the original file
       bytes : this is of the GENERATED file
    *)
    | { line_column = None; bytes = Some bytes } ->
        let ic = open_in_bin mlpath in
        let rec iter lines remain =
          let pos = pos_in ic in
          let new_remain = bytes - pos in
          if new_remain < 0 then begin (* run over *)
            close_in ic;
            { line_column = Some (lines, remain); bytes = Some bytes }
          end else begin
            if try ignore (input_line ic); true with End_of_file -> false then
              iter (lines+1) new_remain
            else
              { line_column = Some (lines+1, new_remain); bytes = Some bytes }
          end
        in
        iter 0 bytes

    | { line_column = None; bytes = None } -> assert false

end

module Region : sig

  type t = private {
    start : Position.t;
    end_  : Position.t
  }

  val compare : t -> t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]

  val to_string : t -> string
  val of_parsing : Location.t -> string * t
  val split : t -> by:t -> (t * t) option

  val point_by_byte : int -> t
  (** works only if bytes are available *)

  val point : Position.t -> t
  val length_in_bytes : t -> int
  val is_complete : t -> bool
  val complete : string -> t -> t
  val substring : string -> t -> t * string

end = struct

  open Location
  open Lexing

  (* CR jfuruse: I heard that inode is not a good idea; mingw has no inode *)
  type t = {
    start : Position.t;
    end_  : Position.t
  }

  let to_string t =
    Printf.sprintf "%s:%s"
      (Position.to_string t.start)
      (Position.to_string t.end_)

  let of_parsing l =
    let fname1 = l.loc_start.pos_fname in
    let fname2 = l.loc_end.pos_fname in
    if fname1 <> fname2 then
      Format.eprintf "Warning: A location contains strange file names %s and %s@." fname1 fname2;
    (* Flip locs if they are in opposite order. 
       Actually this never helps. Such strange poses are created by
       buggy P4. *)
    let start = Position.of_lexing_position l.loc_start in
    let end_ = Position.of_lexing_position l.loc_end in
    match Position.compare start end_ with
    | -1 | 0 -> fname1, { start; end_ }
    | _ -> fname1, { start = end_; end_ = start }

  let compare l1 l2 =
    let starts = Position.compare l1.start l2.start in
    let ends   = Position.compare l1.end_  l2.end_  in
    if starts = 0 && ends = 0 then `Same
    else if starts <= 0 && ends >= 0 then `Includes
    else if starts >= 0 && ends <= 0 then `Included
    else if Position.compare l1.end_ l2.start <= 0 then `Left
    else if Position.compare l2.end_ l1.start <= 0 then `Right
    else `Overwrap

  let split l1 ~by:l2 =
    if compare l1 l2 = `Overwrap then
      if Position.compare l1.start l2.start < 0 then
	Some ({ l1 with end_ = (* position_prev *) l2.start },
	      { l1 with start = l2.start })
      else if Position.compare l2.start l1.start < 0 then
        Some ({ l1 with end_ = l2.end_ },
	      { l1 with start = (* position_next *) l2.end_ })
      else assert false
    else None

  open Position

  let point_by_byte pos =
    { start = { line_column = None;
 		bytes = Some pos };
      end_ = { line_column = None;
               bytes = Some (pos + 1)} }

  let point pos =
    { start = pos; end_ = Position.next pos }

  let length_in_bytes t =
    let bytes = function
      | { Position.bytes = Some bytes; _ } -> bytes
      | _ -> raise Not_found
    in
    bytes t.end_ - bytes t.start

  let is_complete t =
    Position.is_complete t.start && Position.is_complete t.end_

  let complete mlpath t =
    { start = Position.complete mlpath t.start;
      end_ =  Position.complete mlpath t.end_ }

  let substring mlpath t =
    let t = complete mlpath t in
    let ic = open_in_bin mlpath in
    match t.start.Position.bytes, t.end_.Position.bytes with
    | Some start, Some end_ ->
	seek_in ic start;
	let s = Bytes.create (end_ - start) in
	really_input ic s 0 (end_ - start);
	t, Bytes.to_string s
    | _ -> assert false

end

module Regioned = struct
  type 'a t = { region: Region.t; value: 'a }

  let compare { region = r1; _ } { region = r2; _ } = Region.compare r1 r2

  let split { region = r1; value = v } ~by:{ region = r2; _ } =
    Option.map (Region.split r1 ~by: r2) ~f:(fun (r11, r12) ->
      { region = r11; value = v },
      { region = r12; value = v })

  let format f ppf { region = r; value = v } =
    fprintf ppf "@[<2>%s:@ @[%a@]@]"
      (Region.to_string r)
      f v
end

module FileRegioned = struct
  type 'a t = { file_region: string * Region.t; value: 'a }

  let format f ppf { file_region = (file,r); value = v } =
    fprintf ppf "@[<2>%s:%s:@ @[%a@]@]"
      file
      (Region.to_string r)
      f v
end

module Tree : sig
  type elem = Annot.t list Regioned.t
  type t
  val empty : t
  val is_empty : t -> bool
(*
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val cardinal : t -> int
  val add : t -> elem -> t
*)
  val of_loc_annots : builddir: string -> path: string -> (Location.t, Annot.t list) Hashtbl.t -> t
  val find_path_contains : Region.t -> t -> (elem * t) list

  val iter : (parent:elem option -> elem -> unit) -> t -> unit
    (** Region splitted Annot may be itered more than once. *)

  val dump : t -> unit
  val dump2 : t -> unit
end = struct

  (* Tree is for search by location, and it is only meaningful
     for one source file. cmt may contain more than one file path,
     but we stick to the main file path.

     [of_loc_annots ~path loc_annots] does rather ragical simplifiction.
     It throws away the loc_annots with file file basenames from [path]'s.
  *)

  (* annotation with region *)
  module RAnnot = struct
    type t      = Annot.t list Regioned.t
    let split   = Regioned.split
    let compare = Regioned.compare
    let format  = Regioned.format (Format.list ";@ " Annot.format)
  end

  include Treeset.Make(RAnnot)

  open Regioned

  (* If the region maybe splitted, the original region will be gone *)
  let add t rannot = add_elem rannot t

  let of_loc_annots ~builddir ~path loc_annots =
    Hashtbl.fold (fun loc annots st ->
      let fname, region = Region.of_parsing loc in
      match fname with
      | "_none_" -> st
      | _ -> 
          if loc.Location.loc_ghost then st
          else begin
            if path = builddir ^/ fname then
              add st { Regioned.region; value = annots } 
            else begin
              Format.eprintf "Call the Author: Guru meditation: path=%s fname=%s@." path fname;
              add st { Regioned.region; value = annots } 
            end
          end)
      loc_annots empty

  let iter = iter_elem

  let find_path_contains r t =
    let probe = { region = r; value = [] (* dummy *) } in
    find_path_contains probe t

  let dump t =
    iter_elem (fun ~parent rrspot ->
	let format_parent ppf = function
	  | None -> fprintf ppf "ROOT"
	  | Some rrspot -> RAnnot.format ppf rrspot
	in
	eprintf "@[<2>@[%a@] =>@ @[%a@]@]@."
	  format_parent parent
	  RAnnot.format rrspot) t

  let dump2 t =
    let open Format in
    let nodes = Hashtbl.create 1023 in
    iter_elem (fun ~parent rrspot -> 
      Hashtbl.multi_add nodes (Option.map ~f:(fun x -> x.region) parent) rrspot) t;
    let rec loop ppf rrspot =
      fprintf ppf "=> @[<v>%a%a@]" 
        RAnnot.format rrspot
        loop_region (Some rrspot.region);
    and loop_region ppf regopt =
      let subnodes = Hashtbl.find_default [] nodes regopt in
      if subnodes = [] then ()
      else begin
        fprintf ppf "@,  @[<v>%a@]"
          (list "@," loop) subnodes
      end;
    in
    Format.eprintf "@[<v>Root%a@]@." loop_region None;
end

(* Minimum data for spotting, which are saved into spot files *)
module Abs = struct
  open Cmt_format

  let abstraction cmt = match cmt.cmt_annots with
    | Implementation str -> EXTRACT.top_structure str
    | Interface sg -> EXTRACT.top_signature sg
    | Packed (_sg, files) ->
        (List.map (fun file ->
          let fullpath = if Filename.is_relative file then cmt.cmt_builddir ^/ file else file in
          let modname = match Filename.split_extension (Filename.basename file) with
            | modname, (".cmo" | ".cmx" | ".cmi") -> String.capitalize_ascii modname
            | _ -> Format.eprintf "packed module with strange name: %s@." file; assert false
          in
          Abstraction.AStr_module (Ident.create modname (* stamp is bogus *),
                                   Some (Abstraction.AMod_packed fullpath))) files),
        Hashtbl.create 1 (* empty *)
    | Partial_implementation parts | Partial_interface parts -> 
        Format.eprintf "Warning: this file is made from compilation with errors@.";
        EXTRACT.clear_cache ();
        let down_to_sitems = function
          | Abstraction.AMod_structure str -> str
          | _ -> [] (* We cannot extract sitems from the others *)
        in
        let part = function
          | Partial_structure str -> down_to_sitems & EXTRACT.structure str
          | Partial_structure_item sitem -> EXTRACT.structure_item sitem
          | Partial_expression e -> EXTRACT.expression e; []
          | Partial_pattern p -> EXTRACT.pattern p
          | Partial_class_expr cexp -> EXTRACT.class_expr cexp; []
          | Partial_signature sg -> down_to_sitems & EXTRACT.signature sg
          | Partial_signature_item sgitem -> EXTRACT.signature_item sgitem
          | Partial_module_type mty -> down_to_sitems & EXTRACT.module_type mty
        in
        Hashtbl.clear EXTRACT.tbl;
        let tbl = EXTRACT.tbl in (* CR jfuruse: this is global! *)
        let amods = List.concat_map part & Array.to_list parts in
        amods,
        tbl

  let abstraction cmt =
    let load_path = List.map (fun p ->
      cmt.cmt_builddir ^/ p) cmt.cmt_loadpath
    in
    with_ref Config.load_path load_path (fun () ->
      try abstraction cmt; with e ->
        Format.eprintf "Aiee %s@." (Printexc.to_string e);
        raise e)

end

(* Spot info for each compilation unit *)
module Unit = struct

  type t = {
    modname        : string;
    builddir       : string;
    loadpath       : string list;
    args           : string array;
    path           : string; (** source path. If packed, the .cmo itself *)
    top            : Abstraction.structure;
    loc_annots     : (Location.t, Annot.t list) Hashtbl.t;

    flat           : Abstraction.structure lazy_t;
    id_def_regions : (Ident.t, (string * Region.t)) Hashtbl.t lazy_t;
    rannots        : Annot.t list FileRegioned.t list lazy_t;
    tree           : Tree.t lazy_t;
    
    top_signature      : Types.signature option;
  }

  (* same as F.dump, ignoring new additions in Unit *)
  let dump file =
    eprintf "@[<v2>{ module= %S;@ path= %S;@ builddir= %S;@ loadpath= [ @[%a@] ];@ argv= [| @[%a@] |];@ ... }@]@."
      file.modname
      file.path
      file.builddir
      (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) file.loadpath
      (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) (Array.to_list file.args)

  let of_cmt path (* the cmt file path *) cmt =
    let path = Option.default (Cmt.source_path cmt) (fun () ->
      let ext = if Cmt.is_opt cmt then ".cmx" else ".cmo" in
      Filename.chop_extension path ^ ext)
    in
(*
Format.eprintf "Spot.Tree.of_cmt path=%s digest=%s@." 
  path
  (match cmt.Cmt_format.cmt_source_digest with
  | None -> "None"
  | Some s -> Digest.to_hex s)
    ;
*)
    let top, loc_annots = Abs.abstraction cmt in
    
    let rannots = lazy begin
      Hashtbl.fold (fun loc annots st ->
        { FileRegioned.file_region = Region.of_parsing loc;  
          value = annots } :: st
      ) loc_annots []
    end in
    let id_def_regions = lazy (
      let tbl = Hashtbl.create 1023 in
      Hashtbl.iter (fun loc annots ->
        List.iter (function
          | Annot.Str_item sitem ->
              let _kind,id = Abstraction.ident_of_structure_item sitem in
              Hashtbl.add tbl id (let (file, r) = Region.of_parsing loc in
                                  cmt.cmt_builddir ^/ file,r)
          | _ -> ()) annots) loc_annots;
      tbl)
    in
    let tree = lazy begin
      Tree.of_loc_annots ~builddir: cmt.cmt_builddir ~path:path loc_annots
    end in

    (* CR jfuruse: it is almost the same as id_def_regions_list *)
    let flat = lazy (Hashtbl.fold (fun _loc annots st ->
      List.filter_map (function
        | Annot.Str_item sitem -> Some sitem
        | _ -> None) annots @ st) loc_annots [])
    in
    
    let top_signature = match cmt.cmt_annots with
      | Implementation str -> Some str.str_type
      | Interface sg -> Some sg.sig_type
      | Packed (sg, _files) -> Some sg
      | Partial_implementation _parts | Partial_interface _parts -> None
    in

    { modname    = cmt.cmt_modname;
      builddir   = cmt.cmt_builddir;
      loadpath   = cmt.cmt_loadpath;
      args       = cmt.cmt_args;
      path       = path;
      top        = top;
      loc_annots = loc_annots;

      flat; id_def_regions; rannots; tree;

      top_signature
    }
end
