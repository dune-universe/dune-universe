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

let rec find_map_opt f list =
  match list with
  | [] -> None
  | hd :: tl ->
      match f hd with
      | None -> find_map_opt f tl
      | result -> result

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
  [%meta if Sys.ocaml_version >= "4.09.0" then
    [%e Compmisc.init_path ()]
  else
    [%e Compmisc.init_path false]];
  Compmisc.initial_env ()
)

let try_find_module ~loc:_loc env lid =
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
    let path =
      [%meta if Sys.ocaml_version >= "4.10.0" then
        [%e fst (Env.lookup_module ~loc:_loc lid env)]
      else
        [%e Env.lookup_module ~load:true lid env]] in
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

module String_map = Map.Make (String)

module String_set = Set.Make (String)

module Longident_map = Map.Make (struct
  type t = Longident.t
  let compare = compare
end)

exception Unsupported

let rec equal_core_type (t0 : Parsetree.core_type) (t1 : Parsetree.core_type) =
  Core_type_equiv.equiv_core_type equal_core_type t0 t1

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
      Core_type_equiv.equiv_core_type (match_core_type subst_ref) p t

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

let rec rewrite_mod (subst : Longident.t Longident_map.t) (lid : Longident.t) =
  match Longident_map.find_opt lid subst with
  | Some lid' -> lid'
  | None ->
    match lid with
    | Lident _ -> lid
    | Ldot (lid, name) -> Ldot (rewrite_mod subst lid, name)
    | Lapply (u, v) -> Lapply (rewrite_mod subst u, rewrite_mod subst v)

let mapper_of_rewrite_context rewrite_context =
  let typ (mapper : Ast_mapper.mapper) (core_type : Parsetree.core_type) =
    let core_type =
      match core_type.ptyp_desc with
      | Ptyp_var x ->
          begin try String_map.find x rewrite_context.subst_var
          with Not_found -> core_type
          end
      | Ptyp_constr (lid, args) ->
          let args = args |> List.map (mapper.typ mapper) in
          let txt =
            match
              Longident_map.find_opt lid.txt rewrite_context.subst_constr
            with
            | Some txt -> txt
            | None -> rewrite_mod rewrite_context.subst_mod lid.txt in
          { core_type with ptyp_desc = Ptyp_constr ({ lid with txt }, args) }
      | _ -> Ast_mapper.default_mapper.typ mapper core_type in
    rewrite rewrite_context.rewrite_system core_type in
  { Ast_mapper.default_mapper with typ }

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

module Symbol_table = struct
  type 'a group = {
      rec_flag : Asttypes.rec_flag;
      decls : 'a list;
    }

  type type_decl = {
      decl : Parsetree.type_declaration;
      mutable imported : bool;
      mutable rec_group : type_decl group;
    }

  type modtype_decl = {
      decl : Parsetree.module_type_declaration;
      mutable imported : bool;
    }

  type item =
    | Type of type_decl group
    | Modtype of modtype_decl
    | Value of Parsetree.value_description
    | Module of Parsetree.module_declaration group

  let empty_group = { rec_flag = Nonrecursive; decls = [] }

  type t = {
      types : type_decl String_map.t;
      modules : Parsetree.module_declaration String_map.t;
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
    match Metapp.string_option_of_module_name name with
    | None -> table
    | Some name ->
        { table with modules = String_map.add name mod_decl table.modules }

  let add_module_type name mod_decl table =
    { table with
      module_types = String_map.add name mod_decl table.module_types }

  let not_only_types table =
    { table with only_types = false }

  type signature = {
      sig_ : Parsetree.signature;
      items : item list;
      table : t;
    }

  let add_item (rev_items, table) (item : Parsetree.signature_item) =
    match item.psig_desc with
    | Psig_type (rec_flag, decls) ->
        let add_type (rev_decls, table) decl =
          let type_decl = { decl; imported = false; rec_group = empty_group } in
          type_decl :: rev_decls,
          add_type decl.ptype_name.txt type_decl table in
        let rev_decls, table = List.fold_left add_type ([], table) decls in
        let group = { rec_flag; decls = List.rev rev_decls } in
        rev_decls |> List.iter begin fun (decl : type_decl) ->
          decl.rec_group <- group;
        end;
        Type group :: rev_items, table
    | Psig_module decl ->
        let group = { rec_flag = Nonrecursive; decls = [decl] } in
        let table = add_module decl.pmd_name.txt decl table in
        Module group :: rev_items, not_only_types table
    | Psig_recmodule decls ->
        let group = { rec_flag = Recursive; decls = decls } in
        let table = List.fold_left begin
          fun table (decl : Parsetree.module_declaration) ->
            add_module decl.pmd_name.txt decl table
        end table decls in
        Module group :: rev_items, not_only_types table
    | Psig_modtype decl ->
        let mod_decl = { decl; imported = false } in
        let table = add_module_type decl.pmtd_name.txt mod_decl table in
        Modtype mod_decl :: rev_items, table
    | Psig_value desc ->
        Value desc :: rev_items, not_only_types table
    | _ -> rev_items, not_only_types table

  let of_signature sig_ =
    let rev_items, table = List.fold_left add_item ([], empty) sig_ in
    { sig_; items = List.rev rev_items; table }

  let import ~target ~source =
    let take_source _key target source = Some source in
    { types = String_map.union take_source target.types source.types;
      modules = String_map.union take_source target.modules source.modules;
      module_types =
        String_map.union take_source target.module_types source.module_types;
      only_types = target.only_types && source.only_types; }
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

let attr_name_is name (attr : Parsetree.attribute) =
  (Metapp.Attr.name attr).txt = name

let has_attr name attributes =
  attributes |> List.exists (attr_name_is name)

let find_attr_type ~loc name attributes =
  match Zipper.find (attr_name_is name) attributes with
  | None -> None
  | Some zipper ->
      match Metapp.Attr.payload zipper.current with
      | PTyp ty -> Some (zipper, ty)
      | _ -> Location.raise_errorf ~loc "Type expected"

let import_type_declaration ~loc rewrite_context ?modident name
    ?params ?(attrs = [])
    (decl : Parsetree.type_declaration) : Parsetree.type_declaration =
  let mapper = mapper_of_rewrite_context rewrite_context in
  let result = mapper.type_declaration mapper decl in
  let params =
    match params with
    | None -> decl.ptype_params |> List.map fst
    | Some params -> params in
  let from_name = decl.ptype_name in
  let ptype_name = name in
  let ptype_manifest, ptype_attributes =
    match result.ptype_manifest with
    | Some typ ->
        let attrs : Parsetree.attributes =
          if has_attr attr_rewrite attrs && not (has_attr attr_from attrs) then
            let imported_type = Ast_helper.Typ.constr
                (ident_of_name from_name) params in
            Metapp.Attr.mk
              (Parsetree_of_types.mkloc attr_from) (PTyp imported_type) :: attrs
          else
            attrs in
        Some typ, attrs
    | None ->
        let manifest =
          modident |> Option.map begin fun modident ->
            Ast_helper.Typ.constr (qualified_ident_of_name modident from_name)
              params
          end in
        manifest, attrs in
  { result with ptype_name; ptype_manifest; ptype_attributes }

type import_type_decl = {
    new_name : string Location.loc;
    attrs : Parsetree.attributes;
    decl : Symbol_table.type_decl;
    params : Parsetree.core_type list option;
    loc : Location.t;
    pdecl : Parsetree.type_declaration option;
  }

let prepare_import_rewrite_context rewrite_context { new_name; decl } =
  let from_name = decl.decl.ptype_name in
  if Core_type_equiv.equal_loc ( = ) from_name new_name then
    rewrite_context
  else
    { rewrite_context with
      subst_constr = Longident_map.add
        (Lident from_name.txt) (Longident.Lident new_name.txt)
        rewrite_context.subst_constr }

let subst_params ~loc (params : Parsetree.core_type list)
    (args : Parsetree.core_type list)
    (rewrite_context : rewrite_context) =
  let pairs =
    try List.combine params args
    with Invalid_argument _ ->
      Location.raise_errorf ~loc
        "Imported type has %d parameter(s), but %d are passed"
        (List.length params) (List.length args) in
  let add_subst subst_var ((param : Parsetree.core_type), arg) =
    match param.ptyp_desc with
    | Ptyp_any -> subst_var
    | Ptyp_var x -> String_map.add x arg subst_var
    | _ -> raise Unsupported in
  { rewrite_context with
    subst_var = List.fold_left add_subst rewrite_context.subst_var pairs }

let import_type_decl { new_name; attrs; decl; params; loc } modident
    rewrite_context overriden_ref defined_ref =
  Ast_helper.with_default_loc loc @@ fun () ->
    let rewrite_context =
      match params with
      | None -> rewrite_context
      | Some params ->
          subst_params ~loc (decl.decl.ptype_params |> List.map fst) params
            rewrite_context in
    let result = import_type_declaration ~loc rewrite_context ?modident
      new_name ?params ~attrs decl.decl in
    decl.imported <- true;
    defined_ref := Symbol_set.add_type new_name.txt !defined_ref;
    overriden_ref :=
      Symbol_set.add_type decl.decl.ptype_name.txt !overriden_ref;
    result

let import_of_decl ~loc (decl : Symbol_table.type_decl) attrs =
  let new_name = decl.decl.ptype_name in
  { loc; new_name; attrs; decl; params = None; pdecl = None; }

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

type 'a env = {
    env : Env.t;
    scope : Symbol_table.t;
    signature : 'a;
  }

let get_alias_target (modtype : Parsetree.module_type)
    : Longident.t Location.loc option =
  match modtype.pmty_desc with
  | Pmty_ident ident | Pmty_alias ident -> Some ident
  | _ -> None

let get_signature (modtype : Parsetree.module_type)
    : Parsetree.signature option =
  match modtype.pmty_desc with
  | Pmty_signature signature -> Some signature
  | _ -> None

let rec get_module_type ~loc env (ident : Longident.t)
    : Longident.t * Parsetree.module_type option =
  let (_, modtype_opt) as result =
    match ident with
    | Lident name ->
        let modtype_opt =
          match
            match String_map.find_opt name env.scope.modules with
            | None -> None
            | Some decl ->
                if
                  match get_alias_target decl.pmd_type with
                  | None -> true
                  | Some target -> target.txt <> ident
                then
                  Some decl.pmd_type
                else
                  None
          with
          | (Some _) as result -> result
          | None ->
              try_find_module ~loc env.env ident |>
              Option.map Parsetree_of_types.module_type in
        ident, modtype_opt
    | Ldot (ident, name) ->
        let ident, modtype_opt = get_module_type ~loc env ident in
        let modtype_opt =
          Option.bind modtype_opt begin fun modtype ->
            Option.bind (get_signature modtype) begin fun tsig ->
              tsig |> find_map_opt begin
                fun (item : Parsetree.signature_item) ->
                  match item.psig_desc with
                  | Psig_module decl when decl.pmd_name.txt =
                      Metapp.module_name_of_string_option (Some name) ->
                      Some decl.pmd_type
                  | _ -> None
              end
            end
          end in
        Ldot (ident, name), modtype_opt
    | Lapply (ident, arg) ->
        let ident, modtype_opt = get_module_type ~loc env ident in
        let arg, _modtype_opt = get_module_type ~loc env ident in
        let modtype_opt =
          Option.bind modtype_opt begin fun modtype ->
            Metapp.Mty.destruct_functor modtype |>
            Option.map begin fun (_f, result) ->
              result
            end
          end in
        Lapply (ident, arg), modtype_opt in
  match
    match Option.bind modtype_opt get_alias_target with
    | None -> None
    | (Some target) as result ->
        if ident = target.txt then
          None
        else
          result
  with
  | None -> result
  | Some target -> get_module_type ~loc env target.txt

let resolve_alias ~loc (env : Parsetree.module_type env) =
  match get_alias_target env.signature with
  | None -> Some env.signature
  | Some target ->
      snd (get_module_type ~loc env target.txt)

let extract_functor ~loc env lid =
  match
    Option.bind (resolve_alias ~loc env) Metapp.Mty.destruct_functor
  with
  | Some (f, signature) ->
      f, signature
  | None ->
      Location.raise_errorf ~loc "%s: %a is not a functor"
        override_name Printtyp.longident lid

let extract_signature ~loc env lid =
  match Option.bind (resolve_alias ~loc env) get_signature with
  | Some signature -> signature
  | None ->
      Location.raise_errorf ~loc
        "%s: %a is a functor" override_name  Printtyp.longident lid

type modenv = {
    ident : Longident.t Location.loc;
    modtype : Parsetree.module_type env option;
  }

let apply_functor ~loc modenv name =
  let y, modtype =
    match modenv.modtype with
    | None -> None, None
    | Some env ->
        let f, signature =
          extract_functor ~loc env modenv.ident.txt in
        Some f, Some { env with signature } in
  let modenv = {
    ident = modenv.ident |> map_loc
      (fun ident : Longident.t ->
        match name with
        | None ->
            Location.raise_errorf ~loc
              "%s: cannot expand anonymous module" override_name
        | Some name -> Lapply (ident, Lident name));
    modtype } in
  y, modenv

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

let map_typ_constr
    (p : Longident.t Location.loc -> Parsetree.core_type list ->
      Parsetree.core_type) t =
  let typ (mapper : Ast_mapper.mapper) (t : Parsetree.core_type) =
    match t.ptyp_desc with
    | Ptyp_constr (ident, args) ->
        let result = p ident (args |> List.map (mapper.typ mapper)) in
        { t with ptyp_desc = result.ptyp_desc }
    | _ -> Ast_mapper.default_mapper.typ mapper t in
  let mapper = { Ast_mapper.default_mapper with typ } in
  mapper.typ mapper t

let map_typ_constr_ident
    (p : Longident.t Location.loc -> Longident.t Location.loc) t =
  t |> map_typ_constr begin fun ident args ->
    Ast_helper.Typ.constr (p ident) args
  end

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
  | Lapply _ -> invalid_arg "map_ident_leaf"

(*
let rec canonize_type env (context : rewrite_context)
    (ident : Longident.t Location.loc) args =
  let loc = ident.loc in
  match ident.txt with
  | Lident _ -> Ast_helper.Typ.constr ident args
  | Lapply _ -> assert false
  | Ldot (lid, name) ->
      let lid, modtype_opt = get_module_type ~loc env lid in
      match
        Option.bind modtype_opt begin fun modtype ->
          Option.bind (get_signature modtype) begin fun tsig ->
            tsig |> find_map_opt begin fun (item : Parsetree.signature_item) ->
              match item.psig_desc with
              | Psig_type (_, decls) ->
                  begin match
                    decls |> List.find_opt begin
                      fun (decl : Parsetree.type_declaration) ->
                        decl.ptype_name.txt = name
                    end
                  with
                  | None -> None
                  | Some decl ->
                      match decl.ptype_manifest with
                      | None -> None
                      | Some manifest ->
                          Some (decl.ptype_params, manifest)
                  end
              | _ -> None
              end
            end
          end
      with
      | _ (*None*) -> Ast_helper.Typ.constr { loc; txt = Ldot (lid, name) } args
      | Some (params, manifest) ->
          let context =
            subst_params ~loc (params |> List.map fst) args context in
          let mapper = mapper_of_rewrite_context context in
          mapper.typ mapper manifest
*)

let prefix_if_defined_locally prefix
    (defined : Symbol_set.t)
    (type_pattern : Parsetree.core_type) : Parsetree.core_type =
  type_pattern |> map_typ_constr @@ begin fun ident args ->
    match ident.txt |> map_ident_leaf
      (fun mod_name ->
        if String_set.mem mod_name defined.modules then
          Ldot (prefix, mod_name)
        else
          raise Not_found)
      (fun typ_name ->
        if String_set.mem typ_name defined.types then
          Ldot (prefix, typ_name)
        else
          raise Not_found) with
    | exception Not_found -> Ast_helper.Typ.constr ident args
    | txt ->
        let ident = { ident with txt } in
        Ast_helper.Typ.constr ident args
  end

let promote_rewrite ~loc env rewrite rewrite_ref prefix rhs_prefix overriden
    defined new_rewrites =
  let prefixed_rewrites = new_rewrites |> List.rev_map begin fun (lhs, rhs) ->
    let lhs =
      prefix_if_defined_locally prefix overriden lhs in
    let rhs =
      if rhs_prefix then prefix_if_defined_locally prefix
          defined rhs
      else rhs in
    (lhs, rhs)
  end in
  rewrite_ref := List.rev_append prefixed_rewrites !rewrite_ref

type rewrite_env = {
    context : rewrite_context;
    rewrite_system_ref : rewrite_system ref;
    subst_mod_ref : Longident.t Longident_map.t ref;
  }

let current_rewrite_context (env : rewrite_env) =
  { env.context with rewrite_system =
    List.rev_append !(env.rewrite_system_ref) env.context.rewrite_system;
    subst_mod = Longident_map.union (fun _ x y -> Some x) !(env.subst_mod_ref) env.context.subst_mod  }

let make_rewrite_env context =
  { context; rewrite_system_ref = ref []; subst_mod_ref = ref Longident_map.empty }

let derive_rewrite_env (env : rewrite_env) =
  make_rewrite_env (current_rewrite_context env)

let force_rewrite_env rewrite_env =
  match rewrite_env with
  | None -> make_rewrite_env empty_rewrite_context
  | Some rewrite_env -> rewrite_env

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
      | Some decl ->
          let type_name : string Location.loc = { loc; txt = type_name } in
          let qual_name = qualified_ident_of_name modident.txt type_name in
          let params = decl.decl.ptype_params in
          let manifest =
            Ast_helper.Typ.constr qual_name (params |> List.map fst) in
          let ty = Ast_helper.Type.mk ~params ~manifest type_name in
          Metapp.With.typesubst ty :: accu
    end symbols.types [] in
  let module_constraints =
    String_set.fold begin
      fun mod_name accu : Parsetree.with_constraint list ->
        match String_map.find_opt mod_name table.modules with
        | None -> accu
        | Some typed_decl ->
            let mod_name : string Location.loc = { loc; txt = mod_name } in
            let qual_name = qualified_ident_of_name modident.txt mod_name in
            Metapp.With.modsubst (Metapp.lid_of_str mod_name) qual_name :: accu
    end symbols.modules type_constraints in
  module_constraints

let apply_rewrite_attr ~loc ?modident rewrite_system_ref type_decls =
  type_decls |> List.filter_map begin
    fun (decl : Parsetree.type_declaration) ->
      match Zipper.find (attr_name_is attr_rewrite) decl.ptype_attributes with
      | Some ({ current; _ } as zipper)
        when Metapp.Attr.payload current = PStr [] ->
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
                let rule =
                  if has_attr "rhs_to_lhs" decl.ptype_attributes then
                    rhs, decl_pattern
                  else
                    decl_pattern, rhs in
                rewrite_system_ref := rule
                  :: !rewrite_system_ref;
                None
              else
                let ptype_attributes = Zipper.pop zipper in
                let lhs, ptype_attributes =
                  match
                    find_attr_type ~loc:decl.ptype_loc attr_from
                      ptype_attributes
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
                      lhs, ptype_attributes in
                rewrite_system_ref := (lhs, decl_pattern)
                  :: !rewrite_system_ref;
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
    { new_name = pdecl.ptype_name; attrs; decl; pdecl = Some pdecl;
      loc; params = Some (List.map fst pdecl.ptype_params) }
  end

let include_co_in_type_list attrs type_list =
  let types_already_there =
    List.fold_left (fun set import ->
      String_set.add import.decl.decl.ptype_name.txt set)
      String_set.empty type_list in
  let type_list, _types_already_there =
    List.fold_left begin fun accu import ->
      List.fold_left begin fun accu (decl : Symbol_table.type_decl) ->
        let type_list, types_already_there = accu in
        if String_set.mem decl.decl.ptype_name.txt types_already_there then
          accu
        else
          import_of_decl ~loc:import.loc decl attrs :: type_list,
          String_set.add decl.decl.ptype_name.txt types_already_there
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
      let rewrite_context =
        List.fold_left prepare_import_rewrite_context rewrite_context
          type_list in
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
            List.map (fun { decl; loc; pdecl; _ } ->
              (decl.decl.ptype_name, Some decl, loc, pdecl)) in
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
                      match decl.decl.ptype_manifest with
                      | None ->
                          Location.raise_errorf ~loc "Manifest expected"
                      | Some typ ->
                          typ, decl.decl.ptype_params in
            let from_type =
              let mapper = mapper_of_rewrite_context rewrite_context in
              mapper.typ mapper from_type in
            overriden_ref := Symbol_set.add_type name.txt !overriden_ref;
            defined_ref := Symbol_set.add_type name.txt !defined_ref;
            let attrs =
              match pdecl with
              | None -> []
              | Some decl -> decl.ptype_attributes in
            Ast_helper.Type.mk name ~params ~attrs:(([
              Metapp.Attr.mk
                (Parsetree_of_types.mkloc attr_from) (PTyp from_type);
              Metapp.Attr.mk
                (Parsetree_of_types.mkloc attr_rewrite) (PStr []);
              Metapp.Attr.mk
                (Parsetree_of_types.mkloc attr_remove) (PStr [])]) @ attrs)
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

let symbols_only_allowed_in_signatures ~loc () =
  Location.raise_errorf ~loc "[%%symbols] only allowed in signatures"

let keep_module (symbols : Symbol_set.t) (decl : Parsetree.module_declaration) =
  match Metapp.string_option_of_module_name decl.pmd_name.txt with
  | Some name -> not (String_set.mem name symbols.modules)
  | _ -> true

let filter_signature (sig_ : Parsetree.signature) (symbols : Symbol_set.t)
    : Parsetree.signature =
  sig_ |> List.filter_map begin fun (item : Parsetree.signature_item) ->
    match item.psig_desc with
    | Psig_type (rec_flag, decls) ->
        begin match
          decls |> List.filter begin fun (decl : Parsetree.type_declaration) ->
            not (String_set.mem decl.ptype_name.txt symbols.types)
          end
        with
        | [] -> None
        | decls -> Some { item with psig_desc = Psig_type (rec_flag, decls) }
        end
    | Psig_module decl ->
        if keep_module symbols decl then
          None
        else
          Some item
    | Psig_recmodule decls ->
        begin match List.filter (keep_module symbols) decls with
        | [] -> None
        | decls -> Some { item with psig_desc = Psig_recmodule decls }
        end
    | Psig_modtype decl ->
        if String_set.mem decl.pmtd_name.txt symbols.module_types then
          None
        else
          Some item
    | _ -> Some item
  end

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

  let import_modtype_decl ~loc rewrite_context
      (modtype_decl : Parsetree.module_type_declaration) =
    let mapper = mapper_of_rewrite_context rewrite_context in
    let modtype_decl =
      modtype_decl |> mapper.module_type_declaration mapper in
    Wrapper.build { loc; txt = Modtype modtype_decl }

  let override ~loc (rewrite_env : rewrite_env)
      (context : override_context) override_item item bind_item include_item =
    let item = override_item context item in
    let result =
      if context.mode.submodule then
        bind_item item
      else
        include_item item in
    promote_rewrite ~loc context.modenv.modtype
      rewrite_env.context
      rewrite_env.rewrite_system_ref
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
      Wrapper.empty ()
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
            let rev_override_attrs, rev_other_attrs =
              attrs |> List.fold_left begin
                fun (rev_override_attrs, rev_other_attrs)
                    (attr : Parsetree.attribute) ->
                  let attr_name = (Metapp.Attr.name attr).txt in
                  if attr_name = attr_from || attr_name = attr_rewrite
                || attr_name = attr_remove then
                    (attr :: rev_override_attrs, rev_other_attrs)
                  else
                    (rev_override_attrs, attr :: rev_other_attrs)
              end ([], []) in
            let override_attrs = List.rev rev_override_attrs in
            let other_attrs = List.rev rev_other_attrs in
            begin match
              decl_of_list ~loc override_attrs modident
                rewrite_context group.decls context.overriden_ref
                context.defined_ref with
            | [] -> None
            | hd :: tl ->
                let ptype_attributes = other_attrs @ hd.ptype_attributes in
                let decls = { hd with ptype_attributes } :: tl in
                Some (mk_type ~loc context group.rec_flag decls)
            end
        | Modtype decl ->
            if decl.imported then
              None
            else
              let item =
                import_modtype_decl ~loc rewrite_context decl.decl in
              decl.imported <- true;
              Some item
        | Value decl ->
            if only_types then
              None
            else
              let item =
                Wrapper.choose (symbols_only_allowed_in_signatures ~loc)
                (fun () ->
                  let mapper = mapper_of_rewrite_context rewrite_context in
                  Ast_helper.Sig.value
                    (mapper.value_description mapper decl)) in
              Some item
        | Module group ->
            if only_types then
              None
            else
              let item =
                Wrapper.choose (symbols_only_allowed_in_signatures ~loc)
                  begin fun () ->
                    let mapper = mapper_of_rewrite_context rewrite_context in
                    let decls =
                      group.decls |>
                      List.map (mapper.module_declaration mapper) in
                    match group.rec_flag with
                    | Recursive -> Ast_helper.Sig.rec_module decls
                    | Nonrecursive ->
                        match decls with
                        | [decl] -> Ast_helper.Sig.module_ decl
                        | _ -> assert false
                  end in
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
            context.modenv.modtype |> Option.map begin fun env ->
              let signature =
                extract_signature ~loc env context.modenv.ident.txt |>
                Symbol_table.of_signature in
              let scope =
                Symbol_table.import ~target:env.scope ~source:signature.table in
              { env with signature; scope }
            end in
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
            | Include, Some { signature; _ } ->
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
                  let symbols =
                    Symbol_set.union !(context.overriden_ref)
                      !(context.defined_ref) in
                  let sig_constraint =
                    if String_set.is_empty symbols.module_types then
                      with_constraints signature.table context.modenv.ident
                        rewrite_context symbols |> make_with_constraints
                    else
                      let s = filter_signature signature.sig_ symbols in
                      let s =
                        Ast_helper.Sig.open_
                          (Ast_helper.Opn.mk context.modenv.ident) :: s in
                      Some (Ast_helper.Mty.signature s) in
                  make_include sig_constraint :: contents in
          structure_of_contents ~loc contents
        | Functor (f, e) ->
            let context =
              match context.mode.import with
              | Ignore -> context
              | _ ->
                let y, modenv =
                  match f with
                  | Unit ->
                      apply_functor ~loc context.modenv None
                  | Named (x, _) ->
                      apply_functor ~loc context.modenv x.txt in
                let rewrite_env =
                  match f, y with
                  | Named ({ txt = Some x; _ }, _),
                    Some (Named ({ txt = Some y }, _)) ->
                      { context.rewrite_env with context =
                        { context.rewrite_env.context with subst_mod =
                          Longident_map.add (Longident.Lident y)
                            (Longident.Lident x)
                            context.rewrite_env.context.subst_mod }}
                  | _ -> context.rewrite_env in
                { context with modenv; rewrite_env } in
            let e' = override_module_expr context e in
            if context.mode.submodule then
              Wrapper.build_module_expr (Wrapper.mkattr ~loc (
                Wrapper.Functor (f, e')))
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
      (env : Symbol_table.signature env option)
      (contents : Wrapper.contents) =
    contents |> flatten_map begin fun (item : Wrapper.item) ->
      let item_desc = Wrapper.destruct item in
      let loc = item_desc.loc in
      match item_desc.txt, env with
      | Type (rec_flag, type_decls), Some { signature; _ } ->
          let rewrite_context = current_rewrite_context context.rewrite_env in
          prepare_type_decls signature.table.types type_decls
            context.modenv.ident.txt (mk_type ~loc context rec_flag)
            context.overriden_ref context.defined_ref rewrite_context
            ~manifest:context.manifest
      | Module binding, _ ->
          let desc = Wrapper.destruct_module_binding binding in
          let mode = { import = Ignore; submodule = true } in
          [override_submodule context env mode (Module desc) []]
      | Modtype declaration, Some { signature; _ } ->
          if
            String_map.mem declaration.pmtd_name.txt
              signature.table.module_types then
            begin
              context.overriden_ref :=
                Symbol_set.add_module_type declaration.pmtd_name.txt
                  !(context.overriden_ref)
            end;
          [item]
      | Extension ((
          { txt = "rewrite_module"; _ },
            PStr [%str [%e? lhs] = [%e? rhs]]), attrs), _ ->
          let get_construct (e : Parsetree.expression) =
            match e.pexp_desc with
            | Pexp_construct (lid, None) -> lid.txt
            | _ ->
              Location.raise_errorf ~loc:e.pexp_loc "Module name expected" in
          let lhs = get_construct lhs in
          let rhs = get_construct rhs in
          context.rewrite_env.subst_mod_ref :=
            Longident_map.add lhs rhs
            !(context.rewrite_env.subst_mod_ref);
          []
      | Extension (({ txt = "types"; _ }, PStr []), attrs),
        Some { env; signature; _ } ->
          import_symbols_from_signature ~loc ~only_types:true context attrs env
            signature
      | Extension (({ txt = "symbols"; _ }, PStr []), attrs),
        Some { env; signature; _ } ->
          import_symbols_from_signature ~loc ~only_types:false context attrs env
            signature
      | Extension (({ txt = "rewrite"; _ }, payload), attrs), _ ->
          let context = { context with
            rewrite_env = derive_rewrite_env context.rewrite_env } in
          Wrapper.destruct_payload ~loc payload |>
          override_contents context env
      | Extension (({ txt = "recursive"; _ }, payload), attrs), _ ->
          let contents =
            Wrapper.destruct_payload ~loc payload |>
            override_contents context env in
          if context.modenv.modtype = None then (* ocamldep *)
            contents
          else
            Option.to_list (make_recursive ~loc contents attrs)
      | Extension (({ txt = "print_rewrite_system"; _ }, _payload), _attrs),
            _ ->
          let rewrite_context = current_rewrite_context context.rewrite_env in
          rewrite_context.rewrite_system |> List.iter (fun (lhs, rhs) ->
            Format.fprintf Format.err_formatter "%a -> %a@."
              Pprintast.core_type lhs
              Pprintast.core_type rhs);
          []
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
              [override_submodule context env mode submodule attrs]
          end
      | _ -> [item]
    end

  and override_submodule (context : override_context)
      (env : Symbol_table.signature env option)
      (mode : mode) (submodule : module_or_modtype) attrs =
    let name, add_module, find_module, override_module =
      match submodule with
      | Module desc ->
          desc.txt.contents.name,
          Symbol_set.add_module,
          (fun name (signature : Symbol_table.signature) ->
            (find_module name signature.table.modules
              context.modenv.ident.txt).pmd_type),
          (fun context' ->
            override_module context.rewrite_env context' desc)
      | Modtype decl ->
          Metapp.map_loc Option.some decl.pmtd_name,
          Symbol_set.add_module_type,
          (fun name (signature : Symbol_table.signature) ->
            match (find_module_type name signature.table.module_types
              context.modenv.ident.txt).decl.pmtd_type with
            | None ->
                abstract_module_types_not_supported ~loc:decl.pmtd_loc
            | Some modtype -> modtype),
          (fun context' ->
            override_module_type context.rewrite_env context' decl) in
    begin match name.txt with
    | Some name ->
        context.overriden_ref :=
          add_module name !(context.overriden_ref);
    | None -> ()
    end;
    let name_txt =
      match name.txt with
      | None ->
          Location.raise_errorf ~loc:!Ast_helper.default_loc
            "%s: cannot find anonymous module" override_name
      | Some txt -> txt in
    let submodenv =
      match mode.import with
      | Ignore ->
          context.modenv
      | Include | Not_include ->
          let modtype =
            match env with
            | None -> None
            | Some env ->
                Some { env with
                  signature = find_module { name with txt = name_txt }
                    env.signature } in
          { ident = context.modenv.ident |> map_loc
            (fun ident : Longident.t -> Ldot (ident, name_txt));
            modtype } in
    let defined_ref =
      if mode.submodule then
        None
      else
        Some context.defined_ref in
    let context' =
      make_context ?defined_ref ~manifest:context.manifest submodenv name_txt
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
        | None -> Wrapper.empty ()
        | Some item -> item
        end
    | Extension (({ txt = "print_rewrite_system"; _ }, _payload), _attrs) ->
        let rewrite_env = force_rewrite_env context.rewrite_env in
        let rewrite_context = current_rewrite_context rewrite_env in
        rewrite_context.rewrite_system |> List.iter (fun (lhs, rhs) ->
          Format.fprintf Format.err_formatter "%a -> %a@."
            Pprintast.core_type lhs
            Pprintast.core_type rhs);
        Wrapper.empty ()
    | Extension ((extension_name, payload), attrs) ->
        begin match mode_of_string extension_name.txt with
        | exception (Invalid_argument _) -> item
        | mode ->
            match module_or_modtype_of_payload ~loc payload with
            | Module desc ->
                let name = desc.txt.contents.name in
                let name =
                  match name with
                  | { txt = Some txt; _ } -> { name with txt }
                  | _ ->
                      Location.raise_errorf ~loc
                        "%s: anonymous module unsupported here" override_name in
                let rewrite_env = force_rewrite_env context.rewrite_env in
                let modenv =
                  let ident = ident_of_name name in
                  let modtype =
                    if context.ocamldep then
                      None
                    else
                      let env = Lazy.force lazy_env in
                      Some {
                        env;
                        signature =
                          Parsetree_of_types.module_type (locate_sig env ident);
                        scope = Symbol_table.empty } in
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
    (module Migrate_parsetree.OCaml_current)
    (fun config _ ->
      make_mapper {
        ocamldep = config.tool_name = "ocamldep"; rewrite_env = None;
        override_module_type = Signature_mapper.override_module_expr })
