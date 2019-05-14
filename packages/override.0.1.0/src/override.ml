module OCaml_version = Migrate_parsetree.OCaml_407

module To = Migrate_parsetree.Convert
    (Migrate_parsetree.OCaml_current) (OCaml_version)

let convert_arg_label (arg_label : Asttypes.arg_label)
    : OCaml_version.Ast.Asttypes.arg_label =
  match arg_label with
  | Nolabel -> Nolabel
  | Labelled s -> Labelled s
  | Optional s -> Optional s

let convert_closed_flag (closed_flag : Asttypes.closed_flag)
    : OCaml_version.Ast.Asttypes.closed_flag =
  match closed_flag with
  | Closed -> Closed
  | Open -> Open

let convert_mutable_flag (mutable_flag : Asttypes.mutable_flag)
    : OCaml_version.Ast.Asttypes.mutable_flag =
  match mutable_flag with
  | Immutable -> Immutable
  | Mutable -> Mutable

let convert_private_flag (private_flag : Asttypes.private_flag)
    : OCaml_version.Ast.Asttypes.private_flag =
  match private_flag with
  | Private -> Private
  | Public -> Public

let convert_rec_flag (rec_flag : Asttypes.rec_flag)
    : OCaml_version.Ast.Asttypes.rec_flag =
  match rec_flag with
  | Nonrecursive -> Nonrecursive
  | Recursive -> Recursive

let convert_payload (payload : Parsetree.payload)
    : OCaml_version.Ast.Parsetree.payload =
  match payload with
  | PStr s -> PStr (To.copy_structure s)
  | PSig s -> PSig (To.copy_signature s)
  | PPat (p, e)  -> PPat (To.copy_pattern p, Option.map To.copy_expression e)
  | PTyp t -> PTyp (To.copy_core_type t)

let convert_attributes (attributes : Parsetree.attributes)
    : OCaml_version.Ast.Parsetree.attributes =
  attributes |> List.map begin fun (name, payload) ->
    (name, convert_payload payload)
  end

module Ast_mapper = OCaml_version.Ast.Ast_mapper
module Ast_helper = OCaml_version.Ast.Ast_helper
module Parsetree = OCaml_version.Ast.Parsetree

let override_name = "[%%override]"

let recursive_name = "[%%recursive]"

let attr_remove = "remove"

let attr_rewrite = "rewrite"

let attr_from = "from"

let flatten_map f list =
  let rec aux accu list =
    match list with
    | [] -> List.rev accu
    | hd :: tl -> aux (List.rev_append (f hd) accu) tl in
  aux [] list

(*
Adapted from ppx_import
https://github.com/ocaml-ppx/ppx_import/
Copyright (c) 2014 Peter Zotov whitequark@whitequark.org

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

let lazy_env = lazy (
  (* It is important that the typing environment is not evaluated
     right away, but only once the ppx-context has been loaded from
     the AST, so that Config.load_path and the rest of the environment
     context are correctly set.

     The environment setting should happen when reading the
     ppx-context attribute which is the very first structure/signature
     item sent to ppx rewriters. In particular, this happens before
     the [%import ] extensions are traversed, which are the places in
     this code where 'env' is forced.

     We would also have the option to not have a global environment, but
     recompute the typing environment on each [%import ] extension. We don't
     see any advantage in doing this, given that we compute the global/initial
     environment that is the same at all program points.
  *)
  (* We need to set recursive_types manually, because it is not part
     of the context automatically saved by Ast_mapper (as of 4.06),
     and this prevents loading the interface of recursive-types-using
     modules. On the other hand, setting recursive_types more often
     than necessary does not seem harmful. *)
  Clflags.recursive_types := true;
  Compmisc.init_path false;
  Compmisc.initial_env ()
)

let try_find_module ~loc:_ env lid =
  (* Note: we are careful to call `Env.lookup_module` and not
     `Typetexp.lookup_module`, because we want to reason precisely
     about the possible failures: we want to handle the case where
     the module path does not exist, but let all the other errors
     (invalid .cmi format, etc.) bubble up to the error handler.

     `Env.lookup_module` allows to do this easily as it raises
     a well-identified `Not_found` exception, while
     `Typetexp.lookup_module` wraps the Not_found failure in
     user-oriented data and is not meant for catching.

     `Env.find_module` can raise `Not_found` again; we suspect that
     it will not in the cases where `lookup_module` returned correctly,
     but better be safe and bundle them in the same try..with.
  *)
  try
    let path = Env.lookup_module ~load:true lid env in
    let module_decl = Env.find_module path env in
    Some module_decl.md_type
  with Not_found -> None

let try_find_module_type ~loc env lid =
  (* Here again we prefer to handle the `Not_found` case, so we
     use `Env.lookup_module` rather than `Typetexp.lookup_module`. *)
  try
    let _path, modtype_decl = Env.lookup_modtype ~loc lid env in
    Some (match modtype_decl.mtd_type with
        | None ->
          Location.raise_errorf ~loc
            "%s: cannot access the signature of the abstract module %a"
            override_name Printtyp.longident lid
        | Some module_type -> module_type)
  with Not_found -> None

let rec resolve_alias ~loc env (module_type : Types.module_type) =
  match module_type with
  | (Mty_ident path | Mty_alias (_, path) ) ->
    begin
      let module_decl =
        try Env.find_module path env
        with Not_found ->
          Location.raise_errorf ~loc "%s: cannot find module %a"
            override_name Printtyp.path path in
      resolve_alias ~loc env module_decl.md_type
    end
  | _ -> module_type

let locate_sig env (ident : Longident.t Location.loc) =
  match ident with { loc; txt = lid } ->
    match try_find_module ~loc env lid with
    | Some mty -> mty
    | None ->
        match try_find_module_type ~loc env lid with
        | Some mty -> mty
        | None ->
            Location.raise_errorf ~loc "%s: cannot locate module %a"
              override_name Printtyp.longident lid

let rec root_of_longident (lid : Longident.t) =
  match lid with
  | Lident ident -> ident
  | Ldot (lid, _)
  | Lapply (lid, _) -> root_of_longident lid

let is_self_reference (lid : Longident.t) =
  let mn = String.uncapitalize_ascii (root_of_longident lid) in
  let fn = !Location.input_name |> Filename.basename |>
    Filename.chop_extension |> String.uncapitalize_ascii in
  mn = fn

module Int_map = Map.Make (struct
  type t = int
  let compare = compare
end)

module String_map = Map.Make (String)

module String_set = Set.Make (String)

module Longident_map = Map.Make (struct
  type t = Longident.t
  let compare = compare
end)

exception Unsupported

let rec equal_list p l0 l1 =
  match l0, l1 with
  | [], [] -> true
  | hd0 :: tl0, hd1 :: tl1 -> p hd0 hd1 && equal_list p tl0 tl1
  | _ -> false

let equal_loc p (l0 : 'a Location.loc) (l1 : 'a Location.loc) =
  p l0.txt l1.txt

let equal_pair px py (x0, y0) (x1, y1) =
  px x0 x1 && py y0 y1

let equal_payload equal_core_type
    (p0 : Parsetree.payload) (p1 : Parsetree.payload) =
  match p0, p1 with
  | PStr s0, PStr s1 ->
      failwith "TODO [%%override]: equal_payload not implemented for structures"
  | PTyp t0, PTyp t1 -> equal_core_type t0 t1
  | PPat (p0, a0), PPat (p1, a1) ->
      failwith "TODO [%%override]: equal_payload not implemented for patterns"
  | _ -> false

let equal_attributes equal_core_type l0 l1 =
  equal_list (equal_pair ( = ) (equal_payload equal_core_type)) l0 l1

let equal_object_field equal_core_type
    (f0 : Parsetree.object_field) (f1 : Parsetree.object_field) =
  match f0, f1 with
  | Otag (l0, a0, t0), Otag (l1, a1, t1) ->
      l0.txt = l1.txt && equal_attributes equal_core_type a0 a1 &&
      equal_core_type t0 t1
  | Oinherit t0, Oinherit t1 -> equal_core_type t0 t1
  | _ -> false

let equal_row_field equal_core_type
    (f0 : Parsetree.row_field) (f1 : Parsetree.row_field) =
  match f0, f1 with
  | Rtag (l0, a0, b0, t0), Rtag (l1, a1, b1, t1) ->
      l0.txt = l1.txt && equal_attributes equal_core_type a0 a1 && b0 = b1 &&
      equal_list equal_core_type t0 t1
  | Rinherit t0, Rinherit t1 -> equal_core_type t0 t1
  | _ -> false

let equiv_core_type equiv_rec (t0 : Parsetree.core_type)
    (t1 : Parsetree.core_type) =
  equal_attributes equiv_rec t0.ptyp_attributes t1.ptyp_attributes &&
  match t0.ptyp_desc, t1.ptyp_desc with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var x0, Ptyp_var x1 -> x0 = x1
  | Ptyp_arrow (l0, u0, v0), Ptyp_arrow (l1, u1, v1) ->
      l0 = l1 && equiv_rec u0 u1 && equiv_rec v0 v1
  | Ptyp_tuple l0, Ptyp_tuple l1 ->
      equal_list equiv_rec l0 l1
  | Ptyp_constr (c0, a0), Ptyp_constr (c1, a1) ->
      c0.txt = c1.txt && equal_list equiv_rec a0 a1
  | Ptyp_object (f0, c0), Ptyp_object (f1, c1) ->
      c0 = c1 && equal_list (equal_object_field equiv_rec) f0 f1
  | Ptyp_class (c0, t0), Ptyp_class (c1, t1) ->
      c0.txt = c1.txt && equal_list equiv_rec t0 t1
  | Ptyp_alias (t0, x0), Ptyp_alias (t1, x1) ->
      equiv_rec t0 t1 && x0 = x1
  | Ptyp_variant (r0, c0, l0), Ptyp_variant (r1, c1, l1) ->
      equal_list (equal_row_field equiv_rec) r0 r1 && c0 = c1 && l0 = l1
  | Ptyp_poly (x0, t0), Ptyp_poly (x1, t1) ->
      equal_list (equal_loc ( = )) x0 x1 && equiv_rec t0 t1
  | Ptyp_package (p0, l0), Ptyp_package (p1, l1) ->
      p0.txt = p1.txt &&
      equal_list (equal_pair (equal_loc ( = )) equiv_rec) l0 l1
  | Ptyp_extension (x0, e0), Ptyp_extension (x1, e1) ->
      x0.txt = x1.txt && equal_payload equiv_rec e0 e1
  | _ -> false

let rec equal_core_type t0 t1 =
  equiv_core_type equal_core_type t0 t1

let rec match_core_type subst_ref (p : Parsetree.core_type)
    (t : Parsetree.core_type) =
  match p.ptyp_desc with
  | Ptyp_any -> true
  | Ptyp_var x ->
      begin match String_map.find_opt x !subst_ref with
      | Some t' ->
          equal_core_type t t'
      | None ->
          subst_ref := String_map.add x t !subst_ref;
          true
      end
  | _ ->
      equiv_core_type (match_core_type subst_ref) p t

let subst_core_type subst t =
  let typ mapper (t : Parsetree.core_type) =
    match
      match t.ptyp_desc with
      | Ptyp_var x ->
          begin match String_map.find_opt x subst with
          | Some t' -> Some t'
          | None -> None
          end
      | _ -> None
    with
    | Some t' -> t'
    | None -> Ast_mapper.default_mapper.typ mapper t in
  let mapper = { Ast_mapper.default_mapper with typ } in
  mapper.typ mapper t

type rewrite_system = (Parsetree.core_type * Parsetree.core_type) list

let rec rewrite_once (ty : Parsetree.core_type) rewrite_system
    : Parsetree.core_type option =
  match rewrite_system with
  | [] -> None
  | (pat, res) :: tail ->
      let subst_ref = ref String_map.empty in
      match
        if match_core_type subst_ref pat ty then
          let res = subst_core_type !subst_ref res in
          if equal_core_type ty res then
            None
          else
            Some res
        else
          None
      with
      | None -> rewrite_once ty tail
      | res -> res

let rec rewrite rewrite_system (ty : Parsetree.core_type)
    : Parsetree.core_type =
  match ty.ptyp_desc with
  | Ptyp_constr (ident, args) ->
      let args = args |> List.map (rewrite rewrite_system) in
      let new_ty = { ty with ptyp_desc = Ptyp_constr (ident, args)} in
      begin match rewrite_once new_ty rewrite_system with
      | None -> new_ty
      | Some rewritten -> rewrite rewrite_system rewritten
      end
  | _ -> ty

type rewrite_context = {
    subst_var : Parsetree.core_type String_map.t;
    subst_constr : Longident.t Longident_map.t;
    subst_mod : Longident.t Longident_map.t;
    rewrite_system : rewrite_system;
  }

let empty_rewrite_context = {
  subst_var = String_map.empty;
  subst_constr = Longident_map.empty;
  subst_mod = Longident_map.empty;
  rewrite_system = [];
}

type type_conversion_context = {
    rewrite : rewrite_context;
    ancestors : string Lazy.t Int_map.t;
    mutable alias_counter : int;
  }

let create_type_conversion_context rewrite = {
  rewrite;
  ancestors = Int_map.empty;
  alias_counter = 0
}

let mkloc txt : 'a Location.loc = { txt; loc = !Ast_helper.default_loc }

let var_of_type_expr (t : Types.type_expr) =
  match t.desc with
  | Tvar var -> var
  | _ -> invalid_arg "var_of_type_expr"

let univar_of_type_expr (t : Types.type_expr) =
  match t.desc with
  | Tunivar var -> var
  | _ -> invalid_arg "univar_of_type_expr"

let rec rewrite_mod (subst : Longident.t Longident_map.t) (lid : Longident.t) =
  match Longident_map.find_opt lid subst with
  | Some lid' -> lid'
  | None ->
    match lid with
    | Lident _ -> lid
    | Ldot (lid, name) -> Ldot (rewrite_mod subst lid, name)
    | Lapply (u, v) -> Lapply (rewrite_mod subst u, rewrite_mod subst v)

let rec core_type_of_type_expr (context : type_conversion_context)
    (type_expr : Types.type_expr) : Parsetree.core_type =
  match Int_map.find_opt type_expr.id context.ancestors with
  | Some lazy_alias ->
      Ast_helper.Typ.var (Lazy.force lazy_alias)
  | None ->
      let lazy_alias = lazy begin
        let index = context.alias_counter in
        context.alias_counter <- succ index;
        Printf.sprintf "alias%d" index
      end in
      let context = { context with
        ancestors = Int_map.add type_expr.id lazy_alias context.ancestors } in
      let result =
        match type_expr.desc with
        | Tvar None | Tunivar None -> Ast_helper.Typ.any ()
        | Tvar (Some var) | Tunivar (Some var) ->
            begin match String_map.find_opt var context.rewrite.subst_var with
            | Some typ -> typ
            | None -> Ast_helper.Typ.var var
            end
        | Tarrow (label, lhs, rhs, _) ->
            let lhs = core_type_of_type_expr context lhs in
            let lhs =
              match label with
              | Optional _ ->
                  begin match lhs with
                  | [%type: [%t? lhs] option] -> lhs
                  | _ -> assert false
                  end
              | _ -> lhs in
            Ast_helper.Typ.arrow (convert_arg_label label) lhs
              (core_type_of_type_expr context rhs)
        | Ttuple xs ->
            Ast_helper.Typ.tuple (List.map (core_type_of_type_expr context) xs)
        | Tconstr (path, args, _) ->
            let lid = Untypeast.lident_of_path path in
            let args = (List.map (core_type_of_type_expr context) args) in
            let lid =
              match Longident_map.find_opt lid context.rewrite.subst_constr with
              | Some lid' -> lid'
              | None -> lid in
            let lid = rewrite_mod context.rewrite.subst_mod lid in
            let result = Ast_helper.Typ.constr (mkloc lid) args in
            rewrite context.rewrite.rewrite_system result
        | Tvariant { row_fields; _ } ->
            let fields = row_fields |> List.map (convert_row_field context) in
            Ast_helper.Typ.variant fields Closed None
        | Tpoly (ty, tyl) ->
            Ast_helper.Typ.poly
              (List.map
                 (fun ty -> mkloc (Option.get (univar_of_type_expr ty))) tyl)
              (core_type_of_type_expr context ty)
        | Tpackage (path, idl, tyl) ->
            Ast_helper.Typ.package
              (mkloc (Untypeast.lident_of_path path))
              (List.map2
                 (fun id ty -> mkloc id, core_type_of_type_expr context ty)
                 idl tyl)
        | Tobject (fields, cl) ->
            begin match !cl with
            | None ->
                let fields, closed_flag = list_of_fields context [] fields in
                Ast_helper.Typ.object_ fields (convert_closed_flag closed_flag)
            | Some (path, args) ->
                let path = mkloc (Untypeast.lident_of_path path) in
                let args = List.map (core_type_of_type_expr context) args in
                Ast_helper.Typ.class_ path args
             end
        | Tlink ty -> core_type_of_type_expr context ty
        | Tsubst _ | Tnil | Tfield _ -> assert false in
      if Lazy.is_val lazy_alias then
        Ast_helper.Typ.alias result (Lazy.force lazy_alias)
      else
        result

and list_of_fields context accu (type_expr : Types.type_expr)
    : Parsetree.object_field list * Asttypes.closed_flag =
  match type_expr.desc with
  | Tnil -> List.rev accu, Closed
  | Tfield (name, _kind, ty, tail) ->
      list_of_fields context
        (Otag (mkloc name, [], core_type_of_type_expr context ty) :: accu)
        tail
  | Tvar _ -> List.rev accu, Open
  | _ ->
      assert false

and convert_row_field context (label, (row_field : Types.row_field))
    : Parsetree.row_field =
  let label = mkloc label in
  match row_field with
  | Rpresent None -> Rtag (label, [], true, [])
  | Rpresent (Some ttyp) ->
      let args = [core_type_of_type_expr context ttyp] in
      Rtag (label, [], false, args)
  | _ -> assert false

let ptype_params_of_ttype_decl conversion_context
    (ttype_decl : Types.type_declaration) =
  List.map2 (fun param _variance ->
    core_type_of_type_expr conversion_context param,
    (* The equivalent of not specifying the variance explicitly.
       Since the very purpose of ppx_import is to include the full definition,
       it should always be sufficient to rely on the inferencer to deduce
       variance. *)
    OCaml_version.Ast.Asttypes.Invariant)
    ttype_decl.type_params ttype_decl.type_variance

let map_loc f (l : 'a Location.loc) : 'b Location.loc =
  { l with txt = f l.txt }

let ident_of_name name =
  map_loc (fun x : Longident.t -> Lident x) name

let qualified_ident_of_name modident name =
  map_loc (fun x : Longident.t -> Ldot (modident, x)) name

module Symbol_set = struct
  type t = {
    types : String_set.t;
    modules : String_set.t;
    module_types : String_set.t;
  }

  let empty = {
    types = String_set.empty;
    modules = String_set.empty;
    module_types = String_set.empty;
  }

  let add_type type_name symbol_table =
    { symbol_table with types =
      String_set.add type_name symbol_table.types }

  let add_module module_name symbol_table =
    { symbol_table with modules =
      String_set.add module_name symbol_table.modules }

  let add_module_type module_type_name symbol_table =
    { symbol_table with module_types =
      String_set.add module_type_name symbol_table.module_types }

  let union u v = {
    types = String_set.union u.types v.types;
    modules = String_set.union u.modules v.modules;
    module_types = String_set.union u.module_types v.module_types;
  }
end

let type_rec_next (tsig : Types.signature) =
  match tsig with
  | Sig_type (ident, decl, Trec_next) :: tail ->
      Some ((ident, decl), tail)
  | _ -> None

let module_rec_next (tsig : Types.signature) =
  match tsig with
  | Sig_module (ident, decl, Trec_next) :: tail ->
      Some ((ident, decl), tail)
  | _ -> None

let rec cut_sequence cut_item accu sequence =
  match cut_item sequence with
  | Some (item, tail) -> cut_sequence cut_item (item :: accu) tail
  | None -> List.rev accu, sequence

let cut_rec cut_item (rec_status : Types.rec_status) first list
    : Asttypes.rec_flag * 'a * 'b =
  match rec_status with
  | Trec_not -> Nonrecursive, [first], list
  | Trec_first | Trec_next ->
      (* Allow Trec_next here to handle filtered signatures. *)
      let result, tail = cut_sequence cut_item [first] list in
      Recursive, result, tail

module Symbol_table = struct
  type 'a group = {
      rec_flag : OCaml_version.Ast.Asttypes.rec_flag;
      decls : 'a list;
    }

  type type_decl = {
      name : string;
      decl : Types.type_declaration;
      mutable imported : bool;
      mutable rec_group : type_decl group;
    }

  type modtype_decl = {
      name : string;
      decl : Types.modtype_declaration;
      mutable imported : bool;
    }

  type value_decl = {
      name : string;
      desc : Types.value_description;
    }

  type module_decl = {
      name : string;
      decl : Types.module_declaration;
    }

  type item =
    | Type of type_decl group
    | Modtype of modtype_decl
    | Value of value_decl
    | Module of module_decl group

  let empty_group = { rec_flag = Nonrecursive; decls = [] }

  type t = {
      types : type_decl String_map.t;
      modules : module_decl String_map.t;
      module_types : modtype_decl String_map.t;
      only_types : bool;
    }

  let empty = {
    types = String_map.empty;
    modules = String_map.empty;
    module_types = String_map.empty;
    only_types = true;
  }

  let add_type name type_decl table =
    { table with types = String_map.add name type_decl table.types }

  let add_module name mod_decl table =
    { table with modules = String_map.add name mod_decl table.modules }

  let add_module_type name mod_decl table =
    { table with
      module_types = String_map.add name mod_decl table.module_types }

  let not_only_types table =
    { table with only_types = false }

  type signature = {
      tsig : Types.signature;
      items : item list;
      table : t;
    }

  let rec group_signature rev_items table (tsig : Types.signature) =
    match tsig with
    | [] -> List.rev rev_items, table
    | Sig_type (ident, decl, rec_status) :: tail ->
        let rec_flag, group, tail =
          cut_rec type_rec_next rec_status (ident, decl) tail in
        let add_type (rev_decls, table) (ident, decl) =
          let name = Ident.name ident in
          let decl = {
            name; decl; imported = false; rec_group = empty_group } in
          decl :: rev_decls, add_type name decl table in
        let rev_decls, table =
          List.fold_left add_type ([], table) group in
        let group = {
          rec_flag = convert_rec_flag rec_flag;
          decls = List.rev rev_decls } in
        rev_decls |> List.iter begin fun (decl : type_decl) ->
          decl.rec_group <- group;
        end;
        group_signature (Type group :: rev_items) table tail
    | Sig_module (ident, decl, rec_status) :: tail ->
        let rec_flag, group, tail =
          cut_rec module_rec_next rec_status (ident, decl) tail in
        let add_module (rev_decls, table) (ident, decl) =
          let name = Ident.name ident in
          let decl = { name; decl } in
          decl :: rev_decls, add_module name decl table in
        let rev_decls, table =
          List.fold_left add_module ([], table) group in
        let group = {
          rec_flag = convert_rec_flag rec_flag;
          decls = List.rev rev_decls } in
        group_signature (Module group :: rev_items) (not_only_types table) tail
    | Sig_modtype (ident, decl) :: tail ->
        let name = Ident.name ident in
        let decl = { name; decl; imported = false } in
        let table = add_module_type name decl table in
        group_signature (Modtype decl :: rev_items) table tail
    | Sig_value (ident, desc) :: tail ->
        let decl = { name = Ident.name ident; desc } in
        group_signature (Value decl :: rev_items) (not_only_types table) tail
    | _ :: tail -> group_signature rev_items (not_only_types table) tail

  let of_signature tsig =
    let items, table = group_signature [] empty tsig in
    { tsig; items; table }
end

module Zipper = struct
  type 'a t = {
      previous : 'a list;
      current : 'a;
      next : 'a list;
    }

  let rec find previous p list =
    match list with
    | [] -> None
    | current :: next ->
        if p current then
          Some { previous; current; next }
        else
          find (current :: previous) p next

  let find p list =
    find [] p list

  let pop zipper =
    List.rev_append zipper.previous zipper.next
end

let attr_name_is name (({ txt; _}, _) : Parsetree.attribute) =
  txt = name

let has_attr name attributes =
  attributes |> List.exists (attr_name_is name)

let find_attr_type ~loc name attributes =
  match Zipper.find (attr_name_is name) attributes with
  | None -> None
  | Some zipper ->
      match snd zipper.current with
      | PTyp ty -> Some (zipper, ty)
      | _ -> Location.raise_errorf ~loc "Type expected"

let import_label_declaration conversion_context
    (ld : Types.label_declaration) : Parsetree.label_declaration =
  { pld_name = { txt = Ident.name ld.ld_id; loc = ld.ld_loc };
    pld_mutable = convert_mutable_flag ld.ld_mutable;
    pld_type = core_type_of_type_expr conversion_context ld.ld_type;
    pld_loc = ld.ld_loc;
    pld_attributes = convert_attributes ld.ld_attributes; }

let import_constructor_arguments conversion_context
    (arguments : Types.constructor_arguments)
    : Parsetree.constructor_arguments =
  match arguments with
  | Cstr_tuple args ->
      let args = args |>
        List.map (core_type_of_type_expr conversion_context) in
      Pcstr_tuple args
  | Cstr_record labels ->
      let labels = labels |>
        List.map (import_label_declaration conversion_context) in
      Pcstr_record labels

let import_constructor_declaration conversion_context
    (cd : Types.constructor_declaration) : Parsetree.constructor_declaration =
  let pcd_res =
    Option.map (core_type_of_type_expr conversion_context) cd.cd_res in
  { pcd_name = { txt = Ident.name cd.cd_id; loc = cd.cd_loc };
    pcd_args = import_constructor_arguments conversion_context cd.cd_args;
    pcd_res;
    pcd_loc = cd.cd_loc;
    pcd_attributes = convert_attributes cd.cd_attributes; }

let import_type_declaration ~loc conversion_context ?modident name
    ?(from_name = name) ?params ?(attrs = [])
    (decl : Types.type_declaration) : Parsetree.type_declaration =
  let ptype_params = ptype_params_of_ttype_decl conversion_context decl in
  let params =
    match params with
    | None -> List.map fst ptype_params
    | Some params -> params in
  let ptype_kind : Parsetree.type_kind =
    match decl.type_kind with
    | Type_abstract -> Ptype_abstract
    | Type_open -> Ptype_open
    | Type_record (labels, _) ->
        let labels =
          labels |> List.map (import_label_declaration conversion_context) in
        Ptype_record labels
    | Type_variant constrs ->
        let constrs = constrs |>
          List.map (import_constructor_declaration conversion_context) in
      Ptype_variant constrs in
  let ptype_manifest, ptype_attributes =
    match decl.type_manifest with
    | Some typ ->
        let attrs : Parsetree.attributes =
          if has_attr attr_rewrite attrs && not (has_attr attr_from attrs) then
            let imported_type = Ast_helper.Typ.constr
                (ident_of_name from_name)
                params in
            attrs @ [(mkloc attr_from, PTyp imported_type)]
          else
            attrs in
        Some (core_type_of_type_expr conversion_context typ), attrs
    | None ->
        let manifest =
          modident |> Option.map begin fun modident ->
            Ast_helper.Typ.constr (qualified_ident_of_name modident from_name)
              params
          end in
        manifest, attrs in
  { ptype_name = name; ptype_params; ptype_kind; ptype_manifest;
    ptype_cstrs = [];
    ptype_private = convert_private_flag decl.type_private;
    ptype_attributes;
    ptype_loc = decl.type_loc; }

type import_type_decl = {
    from_name : string Location.loc;
    new_name : string Location.loc;
    attrs : Parsetree.attributes;
    decl : Symbol_table.type_decl;
    params : Parsetree.core_type list option;
    loc : Location.t;
    pdecl : Parsetree.type_declaration option;
  }

let import_type_decl { from_name; new_name; attrs; decl; params; loc } modident
    rewrite_context overriden_ref defined_ref =
  Ast_helper.with_default_loc loc @@ fun () ->
  let rewrite_context =
    if from_name = new_name then
      rewrite_context
    else
      { rewrite_context with
        subst_constr = Longident_map.add
          (Lident from_name.txt) (Longident.Lident new_name.txt)
          rewrite_context.subst_constr } in
  let add_subst_param subst_var ((tparam : Types.type_expr), pparam) =
    match var_of_type_expr tparam with
    | Some var -> String_map.add var pparam subst_var
    | None -> subst_var
    | exception (Invalid_argument _) -> raise Unsupported in
  let conversion_context = create_type_conversion_context rewrite_context in
  let conversion_context =
    match params with
    | None -> conversion_context
    | Some params ->
        let pairs =
          try List.combine decl.decl.type_params params
          with Invalid_argument _ ->
            Location.raise_errorf ~loc:new_name.loc
              "Imported type has %d parameter(s), but %d are passed"
              (List.length decl.decl.type_params)
              (List.length params) in
        let conversion_context = { conversion_context with rewrite =
          { conversion_context.rewrite with
            subst_var =
            List.fold_left add_subst_param rewrite_context.subst_var pairs }} in
        conversion_context in
  let result = import_type_declaration ~loc conversion_context ?modident
      new_name ~from_name ?params ~attrs decl.decl in
  decl.imported <- true;
  defined_ref := Symbol_set.add_type new_name.txt !defined_ref;
  overriden_ref := Symbol_set.add_type from_name.txt !overriden_ref;
  result

let import_of_decl ~loc (decl : Symbol_table.type_decl) attrs =
  let name : string Location.loc = { loc; txt = decl.name } in
  { loc; new_name = name; from_name = name; attrs; decl; params = None;
    pdecl = None; }

let decl_of_list ~loc attrs modident rewrite_context
    (decls : Symbol_table.type_decl list) overriden_ref defined_ref =
  decls |> List.filter_map begin fun (decl : Symbol_table.type_decl) ->
    if decl.imported then
      None
    else
      try
        Some (import_type_decl
          (import_of_decl ~loc decl attrs)
          modident rewrite_context overriden_ref defined_ref)
      with Unsupported ->
        None
  end

let not_found kind (name : string Location.loc) ident =
  Location.raise_errorf ~loc:name.loc "%s: %s %s not found in %a"
    override_name kind name.txt Printtyp.longident ident

let find kind (name : string Location.loc) map ident =
  try
    String_map.find name.txt map
  with Not_found ->
    not_found kind name ident

let kind_type = "type"

let find_type arg = find kind_type arg

let find_module arg = find "module" arg

let find_module_type arg = find "module type" arg

type import_mode = Include | Not_include | Ignore

type mode = {
    import : import_mode;
    submodule : bool;
  }

let mode_of_string name =
  match name with
  | "override" -> { import = Include; submodule = true; }
  | "include" -> { import = Include; submodule = false; }
  | "import" -> { import = Not_include; submodule = false; }
  | _ -> invalid_arg "mode_of_string"

let rec remove_prefix prefix (ident : Longident.t) =
  match ident with
  | Lident _ -> ident
  | Ldot (lid, name) ->
      if lid = prefix then Lident name
      else Ldot (remove_prefix prefix lid, name)
  | Lapply (lid, lid') ->
      Lapply (remove_prefix prefix lid, remove_prefix prefix lid')

let map_typ_constr_ident p t =
  let typ (mapper : Ast_mapper.mapper) (t : Parsetree.core_type) =
    match t.ptyp_desc with
    | Ptyp_constr (ident, args) ->
        { t with ptyp_desc =
          Ptyp_constr (p ident, List.map (mapper.typ mapper) args) }
    | _ -> Ast_mapper.default_mapper.typ mapper t in
  let mapper = { Ast_mapper.default_mapper with typ } in
  mapper.typ mapper t

let rec map_ident map_name (ident : Longident.t) : Longident.t =
  match ident with
  | Lident name -> map_name name
  | Ldot (lid, name) -> Ldot (map_ident map_name lid, name)
  | Lapply (lid, lid') ->
      Lapply (map_ident map_name lid, map_ident map_name lid')

let rec map_ident_leaf map_mod_name map_leaf_name (ident : Longident.t)
    : Longident.t =
  match ident with
  | Lident name -> map_leaf_name name
  | Ldot (lid, name) -> Ldot (map_ident map_mod_name lid, name)
  | Lapply _ -> invalid_arg "map_ident_lead"

let prefix_if_defined_locally prefix (defined : Symbol_set.t)
    (type_pattern : Parsetree.core_type) : Parsetree.core_type =
  type_pattern |> map_typ_constr_ident @@ map_loc @@ map_ident_leaf
    (fun mod_name ->
      if String_set.mem mod_name defined.modules then
        Ldot (prefix, mod_name)
      else
        Lident mod_name)
    (fun typ_name ->
      if String_set.mem typ_name defined.types then
        Ldot (prefix, typ_name)
      else
        Lident typ_name)

let promote_rewrite rewrite_ref prefix rhs_prefix overriden defined
    new_rewrites =
  let prefixed_rewrites = new_rewrites |> List.rev_map begin fun (lhs, rhs) ->
    let lhs = prefix_if_defined_locally prefix overriden lhs in
    let rhs =
      if rhs_prefix then prefix_if_defined_locally prefix defined rhs
      else rhs in
    (lhs, rhs)
  end in
  rewrite_ref := List.rev_append prefixed_rewrites !rewrite_ref

type rewrite_env = {
    context : rewrite_context;
    rewrite_system_ref : rewrite_system ref;
  }

let current_rewrite_context (env : rewrite_env) =
  { env.context with rewrite_system =
    List.rev_append !(env.rewrite_system_ref) env.context.rewrite_system }

let make_rewrite_env context =
  { context; rewrite_system_ref = ref [] }

let derive_rewrite_env (env : rewrite_env) =
  make_rewrite_env (current_rewrite_context env)

let force_rewrite_env rewrite_env =
  match rewrite_env with
  | None -> make_rewrite_env empty_rewrite_context
  | Some rewrite_env -> rewrite_env

type modtype = {
    env : Env.t;
    modtype : Types.module_type;
  }

type modenv = {
    ident : Longident.t Location.loc;
    modtype : modtype option;
  }

type override_context = {
    modenv : modenv;
    name : string;
    mode : mode;
    manifest : bool;
    rewrite_env : rewrite_env;
    overriden_ref : Symbol_set.t ref;
    defined_ref : Symbol_set.t ref;
    override_module_type :
      override_context -> Parsetree.module_type -> Parsetree.module_type;
  }

type mapper_context = {
    ocamldep : bool;
    rewrite_env : rewrite_env option;
    override_module_type :
      override_context -> Parsetree.module_type -> Parsetree.module_type;
  }

let get_functor ~loc modenv name =
  let y, modtype =
    match modenv.modtype with
    | None -> None, None
    | Some { env; modtype } ->
        match resolve_alias ~loc env modtype with
        | Mty_functor (y, t, modtype) ->
            Some y, Some { env; modtype }
        | _ ->
            Location.raise_errorf ~loc "%s: %a is not a functor"
              override_name Printtyp.longident modenv.ident.txt in
  let modenv = {
    ident = modenv.ident |> map_loc
      (fun ident : Longident.t -> Lapply (ident, Lident name));
    modtype } in
  y, modenv

let get_signature ~loc modenv =
  match modenv.modtype with
  | None -> None
  | Some { env; modtype } ->
      match resolve_alias ~loc env modtype with
      | Mty_signature s -> Some (env, s)
      | Mty_functor _ ->
          Location.raise_errorf ~loc
            "%s: %a is a functor" override_name Printtyp.longident
            modenv.ident.txt
      | _ -> assert false

let make_context ?(defined_ref = ref Symbol_set.empty) modenv name
    mode ~manifest rewrite_env override_module_type = {
  modenv; name; mode; manifest; rewrite_env;
  overriden_ref = ref Symbol_set.empty;
  defined_ref; override_module_type; }

let with_constraints (table : Symbol_table.t)
    (modident : Longident.t Location.loc) rewrite_context
    (symbols : Symbol_set.t) =
  let loc = modident.loc in
  assert (String_set.is_empty symbols.module_types);
  let type_constraints =
    String_set.fold begin
      fun type_name accu : Parsetree.with_constraint list ->
      match String_map.find_opt type_name table.types with
      | None -> accu
      | Some typed_decl ->
          let type_name : string Location.loc = { loc; txt = type_name } in
          let qual_name = qualified_ident_of_name modident.txt type_name in
          let params =
            ptype_params_of_ttype_decl rewrite_context typed_decl.decl in
          let manifest =
            Ast_helper.Typ.constr qual_name (List.map fst params) in
          let ty = Ast_helper.Type.mk ~params ~manifest type_name in
          Pwith_typesubst (ident_of_name type_name, ty) :: accu
    end symbols.types [] in
  let module_constraints =
    String_set.fold begin
      fun mod_name accu : Parsetree.with_constraint list ->
        match String_map.find_opt mod_name table.modules with
        | None -> accu
        | Some typed_decl ->
            let mod_name : string Location.loc = { loc; txt = mod_name } in
            let qual_name = qualified_ident_of_name modident.txt mod_name in
            Pwith_modsubst (ident_of_name mod_name, qual_name) :: accu
    end symbols.modules type_constraints in
  module_constraints

let apply_rewrite_attr ~loc ?modident rewrite_system_ref type_decls =
  type_decls |> List.filter_map begin
    fun (decl : Parsetree.type_declaration) ->
      match Zipper.find (attr_name_is attr_rewrite) decl.ptype_attributes with
      | Some ({ current = (_, PStr []); _ } as zipper) ->
          begin match rewrite_system_ref with
          | None ->
              Location.raise_errorf ~loc:decl.ptype_loc
                "[@@rewrite] should appear in the scope of [%%override] or [%%import] or [%%include] or [%%rewrite]."
          | Some rewrite_system_ref ->
              let decl_pattern =
                Ast_helper.Typ.constr (ident_of_name decl.ptype_name)
                  (List.map fst decl.ptype_params) in
              if has_attr attr_remove decl.ptype_attributes then
                let rhs =
                  match find_attr_type ~loc:decl.ptype_loc attr_from
                      decl.ptype_attributes with
                  | Some (_zipper, rhs) -> rhs
                  | None -> assert false in
                rewrite_system_ref := (decl_pattern, rhs)
                  :: !rewrite_system_ref;
                None
              else
                let lhs, attributes =
                  match
                    find_attr_type ~loc:decl.ptype_loc attr_from
                      decl.ptype_attributes
                  with
                  | Some (zipper, lhs) ->
                      lhs, Zipper.pop zipper
                  | None ->
                      let lhs =
                        match decl.ptype_manifest with
                        | None ->
                            Location.raise_errorf ~loc:decl.ptype_loc
                              "[@@rewrite] needs a manifest"
                        | Some manifest -> manifest in
                      let lhs =
                        match modident with
                        | None -> lhs
                        | Some modident ->
                            lhs |> map_typ_constr_ident (map_loc (
                              remove_prefix modident)) in
                      lhs, decl.ptype_attributes in
                rewrite_system_ref := (lhs, decl_pattern)
                  :: !rewrite_system_ref;
                let ptype_attributes = Zipper.pop zipper in
                Some { decl with ptype_attributes }
          end
      | _ -> Some decl
  end

let type_decls_has_co (type_decls : Parsetree.type_declaration list) =
  match List.rev type_decls with
  | { ptype_name = { txt = "co"; _ };
      ptype_manifest = None;
      ptype_attributes; _ } :: ((_ :: _) as others)
    when not (has_attr attr_from ptype_attributes) ->
      others, Some ptype_attributes
  | _ -> type_decls, None

let list_type_decls_to_import map modident type_decls =
  type_decls |> List.map begin fun (pdecl : Parsetree.type_declaration) ->
    let loc = pdecl.ptype_loc in
    begin match pdecl.ptype_manifest with
    | Some [%type: _] | None -> ()
    | _ -> Location.raise_errorf ~loc "Types to import should have no manifest"
    end;
    let from_name, attrs =
      match find_attr_type ~loc attr_from pdecl.ptype_attributes with
      | None -> pdecl.ptype_name, pdecl.ptype_attributes
      | Some (zipper, { ptyp_desc =
            Ptyp_constr ({ txt = Lident name; loc }, []); _ }) ->
              { loc; txt = name },
          if has_attr attr_rewrite pdecl.ptype_attributes then
            pdecl.ptype_attributes
          else
            Zipper.pop zipper
      | _ ->
          Location.raise_errorf ~loc "%s: Type name expected" override_name in
    let decl = find_type from_name map modident in
    { from_name; new_name = pdecl.ptype_name; attrs; decl; pdecl = Some pdecl;
      loc; params = Some (List.map fst pdecl.ptype_params) }
  end

let include_co_in_type_list attrs type_list =
  let types_already_there =
    List.fold_left (fun set import -> String_set.add import.from_name.txt set)
      String_set.empty type_list in
  let type_list, _types_already_there =
    List.fold_left begin fun accu import ->
      List.fold_left begin fun accu (decl : Symbol_table.type_decl) ->
        let type_list, types_already_there = accu in
        if String_set.mem decl.name types_already_there then
          accu
        else
          import_of_decl ~loc:import.loc decl attrs :: type_list,
          String_set.add decl.name types_already_there
      end accu import.decl.rec_group.decls
    end (type_list, types_already_there) type_list in
  type_list

let decl_has_attr attr (decl : Parsetree.type_declaration) =
  has_attr attr decl.ptype_attributes

let modident_if_manifest_and_not_self_reference ~manifest modident =
  if not manifest || is_self_reference modident then
    None
  else
    Some modident

let prepare_type_decls map type_decls modident mktype overriden_ref defined_ref
     ~manifest rewrite_context =
  let modident_opt =
    modident_if_manifest_and_not_self_reference ~manifest modident in
  let type_decls', and_co = type_decls_has_co type_decls in
  let type_decls =
    if type_decls |> List.exists begin
      fun (decl : Parsetree.type_declaration) ->
        match decl.ptype_manifest with
        | Some [%type: _] -> true
        | _ -> false
    end then
      let type_list = list_type_decls_to_import map modident type_decls' in
      let type_list =
        match and_co with
        | None -> type_list
        | Some attrs -> include_co_in_type_list attrs type_list in
      type_list |> List.map begin fun import ->
        import_type_decl import modident_opt rewrite_context
          overriden_ref defined_ref
      end
    else if type_decls |> List.exists (decl_has_attr attr_remove) then
      let type_list =
        match and_co with
        | None ->
            type_decls' |> List.map begin
              fun (decl : Parsetree.type_declaration) ->
                decl.ptype_name,
                String_map.find_opt decl.ptype_name.txt map,
                decl.ptype_loc,
                Some decl
            end
        | Some attrs ->
            list_type_decls_to_import map modident type_decls' |>
            include_co_in_type_list attrs |>
            List.map (fun { from_name; decl; loc; pdecl; _ } ->
              (from_name, Some decl, loc, pdecl)) in
      begin
        type_list |> List.iter begin
          fun (_, (decl : Symbol_table.type_decl option), _, _) ->
            match decl with
            | None -> ()
            | Some decl -> decl.imported <- true
        end
      end;
      if type_decls |> List.exists (decl_has_attr attr_rewrite) then
        type_list |> List.map begin
          fun (name, decl, loc,
               (pdecl : Parsetree.type_declaration option)) ->
          Ast_helper.with_default_loc loc begin fun () ->
            let from_type, params =
              match pdecl with
              | Some { ptype_manifest = Some manifest; ptype_params; _ } ->
                  manifest, ptype_params
              | _ ->
                  match decl with
                  | None -> not_found kind_type name modident
                  | Some (decl : Symbol_table.type_decl) ->
                      match decl.decl.type_manifest with
                      | None ->
                          Location.raise_errorf ~loc "Manifest expected"
                      | Some typ ->
                          let conversion_context =
                            create_type_conversion_context rewrite_context in
                          core_type_of_type_expr conversion_context typ,
                          ptype_params_of_ttype_decl conversion_context
                            decl.decl in
            overriden_ref := Symbol_set.add_type name.txt !overriden_ref;
            defined_ref := Symbol_set.add_type name.txt !defined_ref;
            Ast_helper.Type.mk name ~params ~attrs:[
              mkloc attr_from, PTyp from_type;
              mkloc attr_rewrite, PStr [];
              mkloc attr_remove, PStr []]
          end
        end
      else
        []
    else
      begin
        type_decls' |> List.iter begin
          fun (decl : Parsetree.type_declaration) ->
            begin match String_map.find_opt decl.ptype_name.txt map with
            | None -> ()
            | Some decl -> decl.imported <- true
            end;
            overriden_ref :=
              Symbol_set.add_type decl.ptype_name.txt !overriden_ref;
            defined_ref := Symbol_set.add_type decl.ptype_name.txt !defined_ref;
        end;
        type_decls
      end in
  if type_decls = [] then
    []
  else
    [mktype type_decls]

let import_value ~loc rewrite_env name (desc : Types.value_description) =
  let prim =
    match desc.val_kind with
    | Val_prim { prim_name; prim_native_name; _ } ->
        [ prim_name; prim_native_name]
    | _ -> [] in
  let conversion_context = create_type_conversion_context rewrite_env in
  let type_ = core_type_of_type_expr conversion_context desc.val_type in
  Ast_helper.Sig.value ~loc
    (Ast_helper.Val.mk ~loc ~prim { loc; txt = name } type_)

let rec import_signature ~loc rewrite_env (s : Types.signature) =
  match s with
  | [] -> []
  | Sig_value (ident, desc) :: tail ->
      import_value ~loc rewrite_env (Ident.name ident) desc
      :: import_signature ~loc rewrite_env tail
  | Sig_type (ident, decl, rec_status) :: tail ->
      let rec_flag, types, tail =
        cut_rec type_rec_next rec_status (ident, decl) tail in
      let types = types |> List.map begin fun (ident, decl) ->
        let conversion_context = create_type_conversion_context rewrite_env in
        import_type_declaration ~loc conversion_context
          { loc; txt = Ident.name ident } decl
      end in
      Ast_helper.Sig.type_ ~loc Recursive types ::
      import_signature ~loc rewrite_env tail
  | Sig_module (ident, decl, rec_status) :: tail ->
      let rec_flag, modules, tail =
        cut_rec module_rec_next rec_status (ident, decl) tail in
      let decls = modules |> List.map begin
        fun (ident, (decl : Types.module_declaration))
            : Symbol_table.module_decl ->
          { name = Ident.name ident;
            decl = decl; }
      end in
      let rec_flag = convert_rec_flag rec_flag in
      let declarations =
        import_module_group ~loc rewrite_env
          ({ rec_flag; decls } : Symbol_table.module_decl Symbol_table.group) in
      declarations :: import_signature ~loc rewrite_env tail
  | Sig_modtype (ident, decl) :: tail ->
      let typ =
        decl.mtd_type |> Option.map (import_modtype ~loc rewrite_env) in
      let decl =
        Ast_helper.Mtd.mk { loc; txt = Ident.name ident } ?typ in
      Ast_helper.Sig.modtype ~loc decl
        :: import_signature ~loc rewrite_env tail
  | _ ->
      Location.raise_errorf ~loc "Unsupported signature item"

and import_module_group ~loc rewrite_env
    (group : Symbol_table.module_decl Symbol_table.group) =
  let modules = group.decls |> List.map begin
    fun (decl : Symbol_table.module_decl) ->
      let modtype = import_modtype ~loc rewrite_env decl.decl.md_type in
      Ast_helper.Md.mk { loc; txt = decl.name } modtype
  end in
  match group.rec_flag with
  | Nonrecursive ->
      let module_ =
        match modules with
        | [module_] -> module_
        | _ -> assert false in
      Ast_helper.Sig.module_ ~loc module_
  | Recursive ->
      Ast_helper.Sig.rec_module ~loc modules

and import_modtype ~loc rewrite_env (modtype : Types.module_type)
    : Parsetree.module_type =
  match modtype with
  | Mty_ident p ->
      Ast_helper.Mty.ident ~loc { loc ; txt = Untypeast.lident_of_path p }
  | Mty_signature s ->
      Ast_helper.Mty.signature ~loc (import_signature ~loc rewrite_env s)
  | Mty_functor (x, t, s) ->
      let t = t |> Option.map (import_modtype ~loc rewrite_env) in
      let s = import_modtype ~loc rewrite_env s in
      Ast_helper.Mty.functor_ ~loc { loc; txt = Ident.name x } t s
  | Mty_alias (_, p) ->
      Ast_helper.Mty.alias ~loc { loc; txt = Untypeast.lident_of_path p }

let symbols_only_allowed_in_signatures ~loc () =
  Location.raise_errorf ~loc "[%%symbols] only allowed in signatures"

module Make_mapper (Wrapper : Ast_wrapper.S) = struct
  let make_recursive ~loc contents attributes =
    let rec extract_type_decls contents =
      contents |> flatten_map begin fun item ->
        let desc = Wrapper.destruct item in
        match
          match desc.txt with
          | Include inc ->
              begin match
                (Wrapper.destruct_module_expr inc.pincl_mod).txt.contents with
              | Contents contents -> Some (extract_type_decls contents)
              | _ -> None
              end
          | Type (_, type_decls) -> Some type_decls
          | _ -> None
        with
        | Some type_decls -> type_decls
        | None ->
            Location.raise_errorf ~loc:desc.loc
              "%s: Only type declaration expected." recursive_name
    end in
    match extract_type_decls contents with
    | [] -> None
    | hd :: tl ->
        let ptype_attributes = attributes @ hd.ptype_attributes in
        let type_decls = { hd with ptype_attributes } :: tl in
        Some (Wrapper.build { loc; txt = Type (Recursive, type_decls)})

  let include_module ~loc (expr : Wrapper.module_expr) : Wrapper.item =
    Wrapper.build { loc; txt = Include (Ast_helper.Incl.mk ~loc expr)}

  let include_module_type ~loc (modtype : Parsetree.module_type)
      : Wrapper.item =
    let modtype = Wrapper.choose_module_expr
        (fun () ->
          Location.raise_errorf ~loc
            "Module types can only be included in signatures")
        (fun () -> modtype) in
    Wrapper.build { loc; txt = Include (Ast_helper.Incl.mk ~loc modtype)}

  let structure_of_contents ~loc contents =
    Wrapper.build_module_expr (Wrapper.mkattr ~loc (Wrapper.Contents contents))

  let bind_module ~loc name expr =
    Wrapper.build { loc; txt =
      Module (Wrapper.build_module_binding (Wrapper.mkattr ~loc {
        Wrapper.name; expr }))}

  let module_of_ident ~loc ident =
    Wrapper.build_module_expr (Wrapper.mkattr ~loc (Wrapper.Ident ident))

  type module_or_modtype =
    | Module of Wrapper.wrapped_module_binding
    | Modtype of Parsetree.module_type_declaration

  let module_or_modtype_of_payload ~loc payload =
    let payload =
      match Wrapper.destruct_payload ~loc payload with
      | [item] -> Wrapper.destruct item
      | [] -> Location.raise_errorf ~loc "No module given"
      | _ :: _ -> Location.raise_errorf ~loc "Only one module expected" in
    match payload.txt with
    | Module binding -> Module (Wrapper.destruct_module_binding binding)
    | Modtype modtype -> Modtype modtype
    | _ ->
        Location.raise_errorf ~loc:payload.loc "Module or module type expected"

  let abstract_module_types_not_supported ~loc =
    Location.raise_errorf ~loc "Abstract module types are not supported."

  let import_modtype_decl ~loc rewrite_context name
      (modtype_decl : Types.modtype_declaration) =
    let pmtd_type = modtype_decl.mtd_type |>
      Option.map (import_modtype ~loc rewrite_context) in
    Wrapper.build { loc; txt = Modtype {
      pmtd_name = name;
      pmtd_type;
      pmtd_attributes = [];
      pmtd_loc = loc }}

  let override ~loc (rewrite_env : rewrite_env)
      (context : override_context) override_item item bind_item include_item =
    let item = override_item context item in
    let result =
      if context.mode.submodule then
        bind_item item
      else
        include_item item in
    promote_rewrite rewrite_env.rewrite_system_ref
      (Lident context.name) context.mode.submodule
      !(context.overriden_ref) !(context.defined_ref)
      !(context.rewrite_env.rewrite_system_ref);
    result

  let mk_type ~loc context rec_flag type_decls =
    let type_decls =
      apply_rewrite_attr ~loc ~modident:context.modenv.ident.txt
        (Some context.rewrite_env.rewrite_system_ref)
        type_decls in
    if type_decls = [] then
      Wrapper.empty ~loc
    else
      Wrapper.build { loc; txt = Type (rec_flag, type_decls)}

  let import_symbols_from_signature ~loc ~only_types context attrs env
      (signature : Symbol_table.signature) =
    let modident =
      modident_if_manifest_and_not_self_reference
        ~manifest:context.manifest context.modenv.ident.txt in
    let rewrite_context = current_rewrite_context context.rewrite_env in
    signature.items |> List.filter_map begin
      fun (item : Symbol_table.item) ->
        begin match item with
        | Type group ->
            begin match
              decl_of_list ~loc attrs modident
                rewrite_context group.decls context.overriden_ref
                context.defined_ref with
            | [] -> None
            | decls -> Some (mk_type ~loc context group.rec_flag decls)
            end
        | Modtype decl ->
            if decl.imported then
              None
            else
              let item =
                import_modtype_decl ~loc rewrite_context
                  { loc; txt = decl.name } decl.decl in
              decl.imported <- true;
              Some item
        | Value decl ->
            if only_types then
              None
            else
              let item =
                Wrapper.choose (symbols_only_allowed_in_signatures ~loc)
                (fun () ->
                  import_value ~loc rewrite_context decl.name decl.desc) in
              Some item
        | Module group ->
            if only_types then
              None
            else
              let item =
                Wrapper.choose (symbols_only_allowed_in_signatures ~loc)
                (fun () -> import_module_group ~loc rewrite_context group) in
              Some item
        end
    end

  let rec override_module (rewrite_env : rewrite_env)
      (context : override_context) (desc : Wrapper.wrapped_module_binding) =
    let loc = desc.loc in
    override ~loc rewrite_env context
      override_module_expr desc.txt.contents.expr
      (bind_module ~loc desc.txt.contents.name)
      (include_module ~loc)

  and override_module_type (rewrite_env : rewrite_env)
      (context : override_context) (desc : Parsetree.module_type_declaration) =
    let loc = desc.pmtd_loc in
    let mod_type =
      match desc.pmtd_type with
      | None -> abstract_module_types_not_supported ~loc
      | Some mod_type -> mod_type in
    override ~loc rewrite_env context
      context.override_module_type mod_type
      (fun item ->
        let modtypedecl : Parsetree.module_type_declaration = {
          desc with pmtd_type = Some item } in
        Wrapper.build { loc; txt = Modtype modtypedecl })
      (include_module_type ~loc)

  and override_module_expr (context : override_context)
      (expr : Wrapper.module_expr) =
    match Wrapper.destruct_module_expr expr with
      { loc; txt = { attrs; contents }} ->
        match contents with
        | Contents contents ->
          let signature =
            get_signature ~loc context.modenv |>
            Option.map (fun (env, s) -> env, Symbol_table.of_signature s) in
          let contents = override_contents context signature contents in
          let module_expr = module_of_ident ~loc context.modenv.ident in
          let modident =
            Ast_wrapper.module_expr_of_longident context.modenv.ident in
          let type_of () =
            Ast_helper.Mty.typeof_
              (Ast_helper.Mod.structure [
               Ast_helper.Str.include_ (Ast_helper.Incl.mk modident)]) in
          let make_with_constraints with_constraints =
            match with_constraints with
            | [] -> None
            | _ -> Some (Ast_helper.Mty.with_ (type_of ()) with_constraints) in
          let make_include sig_constraint =
            let module_expr =
              match sig_constraint with
              | None ->
                  Wrapper.choose_module_expr (fun () -> modident)
                    (fun () -> type_of ())
              | Some sig_constraint ->
                  Wrapper.build_module_expr (Wrapper.mkattr ~loc (
                    Wrapper.Constraint (
                      Lazy.from_val module_expr, sig_constraint))) in
            include_module ~loc module_expr in
          let contents =
            match context.mode.import, signature with
            | (Not_include | Ignore), _ -> contents
            | Include, None -> make_include None :: contents (* ocamldep *)
            | Include, Some (_env, signature) ->
                if signature.table.only_types &&
                  signature.table.types |> String_map.for_all begin
                    fun _ (decl : Symbol_table.type_decl) ->
                      decl.imported
                  end &&
                  signature.table.module_types |> String_map.for_all begin
                    fun _ (decl : Symbol_table.modtype_decl) ->
                      decl.imported
                  end then
                  contents
                else
                  let rewrite_context =
                    current_rewrite_context context.rewrite_env in
                  let conversion_context =
                    create_type_conversion_context rewrite_context in
                  let symbols =
                    Symbol_set.union !(context.overriden_ref)
                      !(context.defined_ref) in
                  let sig_constraint =
                    if String_set.is_empty symbols.module_types then
                      with_constraints signature.table context.modenv.ident
                        conversion_context symbols |> make_with_constraints
                    else
                      let tsig = signature.tsig |> List.filter begin
                        fun (item : Types.signature_item) ->
                          match item with
                          | Sig_type (ident, _, _) ->
                              not (String_set.mem (Ident.name ident)
                                symbols.types)
                          | Sig_module (ident, _, _) ->
                              not (String_set.mem (Ident.name ident)
                                symbols.modules)
                          | Sig_modtype (ident, _) ->
                              not (String_set.mem (Ident.name ident)
                                symbols.module_types)
                          | _ -> true
                      end in
                      Some (Ast_helper.Mty.signature
                        (import_signature ~loc rewrite_context tsig)) in
                  make_include sig_constraint :: contents in
          structure_of_contents ~loc contents
        | Functor (x, t, e) ->
            let context =
              match context.mode.import with
              | Ignore -> context
              | _ ->
                let y, modenv = get_functor ~loc context.modenv x.txt in
                let rewrite_env =
                  match y with
                  | None -> context.rewrite_env
                  | Some y ->
                      { context.rewrite_env with context =
                        { context.rewrite_env.context with subst_mod =
                          Longident_map.add (Longident.Lident (Ident.name y))
                            (Longident.Lident x.txt)
                            context.rewrite_env.context.subst_mod }} in
                { context with modenv; rewrite_env } in
            let e' = override_module_expr context e in
            if context.mode.submodule then
              Wrapper.build_module_expr (Wrapper.mkattr ~loc (
                Wrapper.Functor (x, t, e')))
            else
              e'
        | Constraint (e, t) ->
            let e' = lazy (override_module_expr context (Lazy.force e)) in
            Wrapper.build_module_expr (Wrapper.mkattr ~loc (
              Wrapper.Constraint (e', t)))
        | _ ->
            Location.raise_errorf ~loc
              "%s: Only functors and structures are supported." override_name

  and override_contents (context : override_context)
      (signature : (Env.t * Symbol_table.signature) option)
      (contents : Wrapper.contents) =
    contents |> flatten_map begin fun (item : Wrapper.item) ->
      let item_desc = Wrapper.destruct item in
      let loc = item_desc.loc in
      match item_desc.txt, signature with
      | Type (rec_flag, type_decls), Some (_env, signature) ->
          let rewrite_context = current_rewrite_context context.rewrite_env in
          prepare_type_decls signature.table.types type_decls
            context.modenv.ident.txt (mk_type ~loc context rec_flag)
            context.overriden_ref context.defined_ref rewrite_context
            ~manifest:context.manifest
      | Module binding, _ ->
          let desc = Wrapper.destruct_module_binding binding in
          let mode = { import = Ignore; submodule = true } in
          [override_submodule context signature mode (Module desc) []]
      | Modtype declaration, Some (_, signature) ->
          if
            String_map.mem declaration.pmtd_name.txt
              signature.table.module_types then
            begin
              context.overriden_ref :=
                Symbol_set.add_module_type declaration.pmtd_name.txt
                  !(context.overriden_ref)
            end;
          [item]
      | Extension (({ txt = "types"; _ }, PStr []), attrs),
        Some (env, signature) ->
          import_symbols_from_signature ~loc ~only_types:true context attrs env
            signature
      | Extension (({ txt = "symbols"; _ }, PStr []), attrs),
        Some (env, signature) ->
          import_symbols_from_signature ~loc ~only_types:false context attrs env
            signature
      | Extension (({ txt = "rewrite"; _ }, payload), attrs), _ ->
          let context = { context with
            rewrite_env = derive_rewrite_env context.rewrite_env } in
          Wrapper.destruct_payload ~loc payload |>
          override_contents context signature
      | Extension (({ txt = "recursive"; _ }, payload), attrs), _ ->
          let contents =
            Wrapper.destruct_payload ~loc payload |>
            override_contents context signature in
          if context.modenv.modtype = None then (* ocamldep *)
            contents
          else
            Option.to_list (make_recursive ~loc contents attrs)
      | Extension ((extension_name, payload), attrs), _ ->
          begin match mode_of_string extension_name.txt with
          | exception (Invalid_argument _) -> [item]
          | mode ->
              let submodule = module_or_modtype_of_payload ~loc payload in
              let manifest =
                match submodule with
                | Module _ -> true
                | Modtype _ -> false in
              let context = { context with manifest } in
              [override_submodule context signature mode submodule attrs]
          end
      | _ -> [item]
    end

  and override_submodule (context : override_context)
      (signature : (Env.t * Symbol_table.signature) option)
      (mode : mode) (submodule : module_or_modtype) attrs =
    let name, add_module, find_module, override_module =
      match submodule with
      | Module desc ->
          desc.txt.contents.name,
          Symbol_set.add_module,
          (fun name (signature : Symbol_table.signature) ->
            (find_module name signature.table.modules
              context.modenv.ident.txt).decl.md_type),
          (fun context' ->
            override_module context.rewrite_env context' desc)
      | Modtype decl ->
          decl.pmtd_name,
          Symbol_set.add_module_type,
          (fun name (signature : Symbol_table.signature) ->
            match (find_module_type name signature.table.module_types
              context.modenv.ident.txt).decl.mtd_type with
            | None ->
                abstract_module_types_not_supported ~loc:decl.pmtd_loc
            | Some modtype -> modtype),
          (fun context' ->
            override_module_type context.rewrite_env context' decl) in
    context.overriden_ref :=
      add_module name.txt !(context.overriden_ref);
    let submodenv =
      match mode.import with
      | Ignore ->
          context.modenv
      | Include | Not_include ->
          let modtype =
            match signature with
            | None -> None
            | Some (env, signature) ->
                let modtype = find_module name signature in
                Some { env; modtype } in
          { ident = context.modenv.ident |> map_loc
            (fun ident : Longident.t -> Ldot (ident, name.txt));
            modtype } in
    let defined_ref =
      if mode.submodule then
        None
      else
        Some context.defined_ref in
    let context' =
      make_context ?defined_ref ~manifest:context.manifest submodenv name.txt
        mode (derive_rewrite_env context.rewrite_env)
        context.override_module_type in
    override_module context'

  let mapper
      (context : mapper_context)
      (mapper : mapper_context -> Ast_mapper.mapper)
      (item : Wrapper.item) =
    let item_desc = Wrapper.destruct item in
    let loc = item_desc.loc in
    let result =
    match item_desc.txt with
    | Extension (({ txt = "rewrite"; _ }, payload), attrs) ->
      let rewrite_env =
        force_rewrite_env context.rewrite_env |>
        derive_rewrite_env in
      let mapper = mapper { context with rewrite_env = Some rewrite_env } in
      let contents =
        Wrapper.destruct_payload ~loc payload |> Wrapper.map mapper mapper in
      include_module ~loc (structure_of_contents ~loc contents)
    | Extension (({ txt = "recursive"; _ }, payload), attrs) ->
      let rewrite_env = force_rewrite_env context.rewrite_env in
      let mapper = mapper { context with rewrite_env = Some rewrite_env } in
      let contents =
        Wrapper.destruct_payload ~loc payload |> Wrapper.map mapper mapper in
      if context.ocamldep then
        include_module ~loc (structure_of_contents ~loc contents)
      else
        begin match make_recursive ~loc contents attrs with
        | None -> Wrapper.empty ~loc
        | Some item -> item
        end
    | Extension ((extension_name, payload), attrs) ->
        begin match mode_of_string extension_name.txt with
        | exception (Invalid_argument _) -> item
        | mode ->
            match module_or_modtype_of_payload ~loc payload with
            | Module desc ->
                let name = desc.txt.contents.name in
                let rewrite_env = force_rewrite_env context.rewrite_env in
                let modenv =
                  let ident = ident_of_name name in
                  let modtype =
                    if context.ocamldep then
                      None
                    else
                      let env = Lazy.force lazy_env in
                      Some { env; modtype = locate_sig env ident } in
                  { ident; modtype } in
                let rewrite_env' = derive_rewrite_env rewrite_env in
                let context =
                  make_context modenv name.txt mode rewrite_env'
                    context.override_module_type ~manifest:true in
                override_module rewrite_env context desc
            | Modtype decl ->
                Location.raise_errorf ~loc
                  "Module types cannot be compilation unit"
        end
    | Type (rec_flag, type_decls) ->
        let rewrite_system_ref =
          context.rewrite_env |> Option.map begin fun env ->
            env.rewrite_system_ref
          end in
        let type_decls =
          apply_rewrite_attr ~loc rewrite_system_ref type_decls in
        Wrapper.build { loc; txt = Type (rec_flag, type_decls)}
    | _ ->
        Wrapper.map_item Ast_mapper.default_mapper (mapper context) item in
    result
end

module Structure_mapper = Make_mapper (Ast_wrapper.Structure)

module Signature_mapper = Make_mapper (Ast_wrapper.Signature)

let rec make_mapper (context : mapper_context) : Ast_mapper.mapper = {
  Ast_mapper.default_mapper with
  structure_item = (fun _mapper -> Structure_mapper.mapper context make_mapper);
  signature_item = (fun _mapper -> Signature_mapper.mapper context make_mapper);
}

let () =
  Migrate_parsetree.Driver.register ~name:"override" ~position:(-10)
    (module OCaml_version)
    (fun config _ ->
      make_mapper {
        ocamldep = config.tool_name = "ocamldep"; rewrite_env = None;
        override_module_type = Signature_mapper.override_module_expr })
