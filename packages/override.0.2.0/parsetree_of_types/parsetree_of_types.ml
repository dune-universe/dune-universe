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
  attributes |> List.map begin fun attribute ->
    match Compat.convert_attribute attribute with
      { attr_name = name; attr_payload = payload; _ } ->
        (name, convert_payload payload)
  end

module Int_map = Map.Make (struct
  type t = int
  let compare = compare
end)

type type_conversion_context = {
    ancestors : string Lazy.t Int_map.t;
    mutable alias_counter : int;
  }

let create_type_conversion_context rewrite = {
  ancestors = Int_map.empty;
  alias_counter = 0
}

let mkloc txt : 'a Location.loc =
  { txt; loc = !OCaml_version.Ast.Ast_helper.default_loc }

let var_of_type_expr (t : Types.type_expr) =
  match t.desc with
  | Tvar var -> var
  | _ -> invalid_arg "var_of_type_expr"

let univar_of_type_expr (t : Types.type_expr) =
  match t.desc with
  | Tunivar var -> var
  | _ -> invalid_arg "univar_of_type_expr"

let rec core_type_of_type_expr (context : type_conversion_context)
    (type_expr : Types.type_expr) : OCaml_version.Ast.Parsetree.core_type =
  match Int_map.find_opt type_expr.id context.ancestors with
  | Some lazy_alias ->
      OCaml_version.Ast.Ast_helper.Typ.var (Lazy.force lazy_alias)
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
        | Tvar None | Tunivar None -> OCaml_version.Ast.Ast_helper.Typ.any ()
        | Tvar (Some var) | Tunivar (Some var) ->
            OCaml_version.Ast.Ast_helper.Typ.var var
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
            OCaml_version.Ast.Ast_helper.Typ.arrow (convert_arg_label label) lhs
              (core_type_of_type_expr context rhs)
        | Ttuple xs ->
            OCaml_version.Ast.Ast_helper.Typ.tuple
              (List.map (core_type_of_type_expr context) xs)
        | Tconstr (path, args, _) ->
            let lid = Untypeast.lident_of_path path in
            let args = (List.map (core_type_of_type_expr context) args) in
            OCaml_version.Ast.Ast_helper.Typ.constr (mkloc lid) args
        | Tvariant { row_fields; _ } ->
            let fields = row_fields |> List.map (convert_row_field context) in
            OCaml_version.Ast.Ast_helper.Typ.variant fields Closed None
        | Tpoly (ty, tyl) ->
            OCaml_version.Ast.Ast_helper.Typ.poly
              (List.map
                 (fun ty -> mkloc (Option.get (univar_of_type_expr ty))) tyl)
              (core_type_of_type_expr context ty)
        | Tpackage (path, idl, tyl) ->
            OCaml_version.Ast.Ast_helper.Typ.package
              (mkloc (Untypeast.lident_of_path path))
              (List.map2
                 (fun id ty -> mkloc id, core_type_of_type_expr context ty)
                 idl tyl)
        | Tobject (fields, cl) ->
            begin match !cl with
            | None ->
                let fields, closed_flag = list_of_fields context [] fields in
                OCaml_version.Ast.Ast_helper.Typ.object_ fields
                  (convert_closed_flag closed_flag)
            | Some (path, args) ->
                let path = mkloc (Untypeast.lident_of_path path) in
                let args = List.map (core_type_of_type_expr context) args in
                OCaml_version.Ast.Ast_helper.Typ.class_ path args
             end
        | Tlink ty -> core_type_of_type_expr context ty
        | Tsubst _ | Tnil | Tfield _ -> assert false in
      if Lazy.is_val lazy_alias then
        OCaml_version.Ast.Ast_helper.Typ.alias result (Lazy.force lazy_alias)
      else
        result

and list_of_fields context accu (type_expr : Types.type_expr)
    : OCaml_version.Ast.Parsetree.object_field list * Asttypes.closed_flag =
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
    : OCaml_version.Ast.Parsetree.row_field =
  let label = mkloc label in
  match row_field with
  | Rpresent None -> Rtag (label, [], true, [])
  | Rpresent (Some ttyp) ->
      let args = [core_type_of_type_expr context ttyp] in
      Rtag (label, [], false, args)
  | _ -> Rtag (label, [], true, [])

let core_type_of_type_expr type_expr =
  core_type_of_type_expr (create_type_conversion_context ()) type_expr

let label_declaration (ld : Types.label_declaration)
    : OCaml_version.Ast.Parsetree.label_declaration =
  { pld_name = { txt = Ident.name ld.ld_id; loc = ld.ld_loc };
    pld_mutable = convert_mutable_flag ld.ld_mutable;
    pld_type = core_type_of_type_expr ld.ld_type;
    pld_loc = ld.ld_loc;
    pld_attributes = convert_attributes ld.ld_attributes; }

let constructor_arguments (arguments : Types.constructor_arguments)
    : OCaml_version.Ast.Parsetree.constructor_arguments =
  match arguments with
  | Cstr_tuple args ->
      let args = args |> List.map core_type_of_type_expr in
      Pcstr_tuple args
  | Cstr_record labels ->
      let labels = labels |> List.map label_declaration in
      Pcstr_record labels

let constructor_declaration (cd : Types.constructor_declaration)
    : OCaml_version.Ast.Parsetree.constructor_declaration =
  let pcd_res = Option.map core_type_of_type_expr cd.cd_res in
  { pcd_name = { txt = Ident.name cd.cd_id; loc = cd.cd_loc };
    pcd_args = constructor_arguments cd.cd_args;
    pcd_res;
    pcd_loc = cd.cd_loc;
    pcd_attributes = convert_attributes cd.cd_attributes; }

let type_declaration name (decl : Types.type_declaration)
    : OCaml_version.Ast.Parsetree.type_declaration =
  let ptype_params = List.map2 begin fun param _variance ->
    core_type_of_type_expr param,
    (* The equivalent of not specifying the variance explicitly.
       Since the very purpose of ppx_import is to include the full definition,
       it should always be sufficient to rely on the inferencer to deduce
       variance. *)
    OCaml_version.Ast.Asttypes.Invariant
  end decl.type_params decl.type_variance in
  let ptype_kind : OCaml_version.Ast.Parsetree.type_kind =
    match decl.type_kind with
    | Type_abstract -> Ptype_abstract
    | Type_open -> Ptype_open
    | Type_record (labels, _) ->
        Ptype_record (labels |> List.map label_declaration)
    | Type_variant constrs ->
        Ptype_variant (constrs |> List.map constructor_declaration) in
  let ptype_manifest =
    decl.type_manifest |> Option.map core_type_of_type_expr in
  { ptype_name = { loc = decl.type_loc; txt = name };
    ptype_params; ptype_kind; ptype_manifest;
    ptype_cstrs = [];
    ptype_private = convert_private_flag decl.type_private;
    ptype_attributes = convert_attributes decl.type_attributes;
    ptype_loc = decl.type_loc; }

let type_rec_next (tsig : Types.signature) =
  match tsig with
  | item :: tail ->
      begin match Compat.convert_signature_item item with
      | Sig_type (ident, decl, Trec_next, _) ->
          Some ((ident, decl), tail)
      | _ -> None
      end
  | _ -> None

let module_rec_next (tsig : Types.signature) =
  match tsig with
  | item :: tail ->
      begin match Compat.convert_signature_item item with
      | Sig_module (ident, _, decl, Trec_next, _) ->
          Some ((ident, decl), tail)
      | _ -> None
      end
  | _ -> None

let rec cut_sequence cut_item accu sequence =
  match cut_item sequence with
  | Some (item, tail) -> cut_sequence cut_item (item :: accu) tail
  | None -> List.rev accu, sequence

let cut_rec cut_item (rec_status : Types.rec_status) first list
    : OCaml_version.Ast.Asttypes.rec_flag * 'a * 'b =
  match rec_status with
  | Trec_not -> Nonrecursive, [first], list
  | Trec_first | Trec_next ->
      (* Allow Trec_next here to handle filtered signatures. *)
      let result, tail = cut_sequence cut_item [first] list in
      Recursive, result, tail

let value_description name (desc : Types.value_description)
    : OCaml_version.Ast.Parsetree.value_description =
  let loc = desc.val_loc in
  let prim =
    match desc.val_kind with
    | Val_prim { prim_name; prim_native_name; _ } ->
        if prim_name = prim_native_name then
          [prim_name]
        else
          [prim_name; prim_native_name]
    | _ -> [] in
  let type_ = core_type_of_type_expr desc.val_type in
  OCaml_version.Ast.Ast_helper.Val.mk ~loc ~prim { loc; txt = name } type_

let rec signature (tsig : Types.signature)
    : OCaml_version.Ast.Parsetree.signature =
  match tsig with
  | [] -> []
  | item :: tail ->
      match Compat.convert_signature_item item with
      | Sig_value (ident, desc, _) ->
          let desc = value_description (Ident.name ident) desc in
          OCaml_version.Ast.Ast_helper.Sig.value desc ::
          signature tail
      | Sig_type (ident, decl, rec_status, _) ->
          let rec_flag, group, tail =
            cut_rec type_rec_next rec_status (ident, decl) tail in
          let group = group |> List.map begin fun (ident, decl) ->
            type_declaration (Ident.name ident) decl
          end in
          OCaml_version.Ast.Ast_helper.Sig.type_ rec_flag group ::
          signature tail
      | Sig_module (ident, _, decl, rec_status, _) ->
          let rec_flag, modules, tail =
            cut_rec module_rec_next rec_status (ident, decl) tail in
          let modules = modules |> List.map module_declaration in
          let item =
            match rec_flag with
            | Nonrecursive ->
                let module_ =
                  match modules with
                  | [module_] -> module_
                  | _ -> assert false in
                OCaml_version.Ast.Ast_helper.Sig.module_ module_
            | Recursive ->
                OCaml_version.Ast.Ast_helper.Sig.rec_module modules in
          item :: signature tail
      | Sig_modtype (ident, decl, _) ->
          OCaml_version.Ast.Ast_helper.Sig.modtype
            (modtype_declaration ident decl) :: signature tail
      | _ ->
          (* TODO: ignored items! *)
          signature tail

and module_declaration (ident, (md : Types.module_declaration)) =
  let loc = md.md_loc in
  OCaml_version.Ast.Ast_helper.Md.mk ~loc
    ~attrs:(convert_attributes md.md_attributes) { loc; txt = Ident.name ident }
    (module_type md.md_type)

and modtype_declaration ident (mtd : Types.modtype_declaration) =
  let loc = mtd.mtd_loc in
  OCaml_version.Ast.Ast_helper.Mtd.mk ~loc
    ~attrs:(convert_attributes mtd.mtd_attributes)
    { loc; txt = Ident.name ident }
    ?typ:(mtd.mtd_type |> Option.map module_type)

and module_type (mt : Types.module_type) =
  match mt with
  | Mty_ident p ->
      OCaml_version.Ast.Ast_helper.Mty.ident
        (mkloc (Untypeast.lident_of_path p))
  | Mty_signature s ->
      OCaml_version.Ast.Ast_helper.Mty.signature (signature s)
  | Mty_functor (x, t, s) ->
      let t = t |> Option.map module_type in
      let s = module_type s in
      OCaml_version.Ast.Ast_helper.Mty.functor_ (mkloc (Ident.name x)) t s
  | Mty_alias _ ->
      match Compat.alias_of_module_type mt with
      | Some p ->
          OCaml_version.Ast.Ast_helper.Mty.alias
            (mkloc (Untypeast.lident_of_path p))
      | None -> assert false
