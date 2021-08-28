open Base
open Ppxlib
open Ocaml_common
module Parse = Ppx_open_parsing.Parse
module Parsed = Ppx_open_parsing.Parsed

let name = "open"
let raise_errorf = Location.raise_errorf

module Module = struct
  include Parsed.Module

  let expand parent_mod_ident ~loc { mod_ident; mod_alias } =
    let ident = mod_alias |> Option.value ~default:mod_ident in
    let (module B) = Ast_builder.make loc in
    let open B in
    let expr = pmod_ident (Located.mk (Ldot (parent_mod_ident, mod_ident))) in
    pstr_module (module_binding ~name:(Located.mk (Some ident)) ~expr)
end

module Module_type = struct
  include Parsed.Module_type

  let expand mod_ident ~loc { mod_type_ident; mod_type_alias } =
    let ident = mod_type_alias |> Option.value ~default:mod_type_ident in
    let (module B) = Ast_builder.make loc in
    let open B in
    let type_ = Some (pmty_ident (Located.mk (Ldot (mod_ident, mod_type_ident)))) in
    pstr_modtype (module_type_declaration ~name:(Located.mk ident) ~type_)
end

module Type = struct
  include Parsed.Type

  let rec string_of_path path =
    let open Path in
    match path with
    | Pident ident -> Ident.name ident
    | Pdot (path, name) -> string_of_path path ^ "." ^ name
    | Papply (path1, path2) -> "(" ^ string_of_path path1 ^ ")" ^ "(" ^ string_of_path path2 ^ ")"


  let rec string_of_lident lident =
    match lident with
    | Lident name -> name
    | Ldot (lident, name) -> string_of_lident lident ^ "." ^ name
    | Lapply (lident1, lident2) ->
      "(" ^ string_of_lident lident1 ^ ")" ^ "(" ^ string_of_lident lident2 ^ ")"


  let string_of_env env =
    Env.diff Env.empty env |> List.map ~f:Ident.name |> String.concat ~sep:", "


  let lident_flatten lident =
    try Longident.flatten lident with
    | _ -> []


  let path_flatten path =
    match Path.flatten path with
    | `Ok (ident, names) -> Some (ident, names)
    | `Contains_apply -> None


  let path_unflatten (ident, names) =
    let open Path in
    let rec loop names =
      match names with
      | [] -> Pident ident
      | name :: names -> Pdot (loop names, name)
    in
    loop (List.rev names)


  let env =
    lazy
      (Compmisc.init_path ();
       Compmisc.initial_env ())


  open Types

  let find_module_type_by_module ~loc env mod_ident =
    let path = Env.lookup_module ~loc mod_ident env |> fst in
    Option.try_with (fun () -> (Env.find_module path env).md_type)


  let find_module_type_by_module_type env mod_ident =
    Option.try_with (fun () ->
        Option.value_exn
          (Env.find_modtype_by_name mod_ident env
          |> fun (_, module_type_decl) -> module_type_decl.mtd_type))


  let find_module_type ~loc env mod_ident =
    match find_module_type_by_module ~loc env mod_ident with
    | Some module_type -> module_type
    | None ->
      (match find_module_type_by_module_type env mod_ident with
      | Some module_type -> module_type
      | None -> raise_errorf ~loc "[%%open]: cannot find module %s" (string_of_lident mod_ident))


  let find_type ~loc path env =
    try Env.find_type path env with
    | (Not_found[@warning "-3"]) ->
      raise_errorf ~loc "[%%open]: cannot find type %s." (string_of_path path)


  let rec signature_of_module_type ~loc env module_type =
    match module_type with
    | Mty_signature signature -> signature
    | Mty_functor _ -> raise_errorf ~loc "[%%open]: cannot access signature of functor."
    | Mty_ident path | Mty_alias path ->
      (match Env.find_module path env with
      | module_decl -> signature_of_module_type ~loc env module_decl.md_type
      | exception (Not_found[@warning "-3"]) ->
        raise_errorf ~loc "[%%open]: cannot find module %s." (string_of_path path))


  let find_type ~loc env mod_ident type_name =
    match lident_flatten mod_ident with
    | head :: names ->
      let mod_type = find_module_type ~loc env (Lident head) in
      let signature = signature_of_module_type ~loc env mod_type in
      let ident = Ident.create_persistent head in
      let env' = Env.add_module ident Mp_present (Mty_signature signature) env in
      find_type ~loc (path_unflatten (ident, names @ [ type_name ])) env'
    | _ ->
      raise_errorf
        ~loc
        "[%%open]: cannot open a functor application %s"
        (string_of_lident mod_ident)


  let rec lident_of_path path =
    let open Path in
    match path with
    | Pident ident -> Lident (Ident.name ident)
    | Pdot (path, name) -> Ldot (lident_of_path path, name)
    | Papply (path1, path2) -> Lapply (lident_of_path path1, lident_of_path path2)


  let rec pcore_type_of_ttype_expr ~loc type_expr =
    let (module B) = Ast_builder.make loc in
    let open B in
    match (Ctype.repr type_expr).desc with
    | Tvar None | Tunivar None ->
      (* Type variables:
        
           Unbound variables may be mapped to [_] type variables, 
           defined by [ptyp_any]. 
        *)
      ptyp_any
    | Tvar (Some tv) | Tunivar (Some tv) ->
      (* Type variables:
        
           Bounded variables may be mapped to their [Parsetree] equivalent. 
        *)
      ptyp_var tv
    | Tarrow (arg_label, lhs, rhs, _) ->
      (* Arrow types (functions):
        
           For arrow types [lhs -> rhs] (with optional arg_label), we simply
           recurively convert the [lhs, rhs] using the [Parsetree] equivalent [ptyp_arrow].

           Note that the [arg_label] must be migrated to it's [Parsetree] equivalent using
           [migrate_arg_label]. 

           Note that we ignore the [commutable] flag (3). TODO: Understand usage.
        *)
      ptyp_arrow
        (Migrate.arg_label arg_label)
        (pcore_type_of_ttype_expr ~loc lhs)
        (pcore_type_of_ttype_expr ~loc rhs)
    | Ttuple tys ->
      (* Tuple type:
        
           As with arrow types, recursively convert the types and then construct
           the tuple type using the [Parsingtree] equivalent [ptyp_tuple].  
        *)
      ptyp_tuple (List.map ~f:(pcore_type_of_ttype_expr ~loc) tys)
    | Tconstr (path, tys, _) ->
      (* Type constructors:
        
           Convert the path to the [Longident.t]. Then recursively convert the applied types.
           
           We ignore the [abbrev_memo ref] value (2) since it
           is used in internal compiler libraries (for tracking known expansions of a type alias). 
           
           Examples: int, int list, ('a, 'b, 'c) Ast_pattern.t
        *)
      let lident = Located.mk (lident_of_path path) in
      ptyp_constr lident (List.map ~f:(pcore_type_of_ttype_expr ~loc) tys)
    | Tvariant { row_fields; row_closed; _ } ->
      (* Polymorphic variants:
        
           Determining the [closed_flag] is simple, since the [row_desc] type
           contains [row_closed] [bool]. 

           [row_fields] is a associative list of [label]s and [row_field] variants:
           - [Rpresent [ty]] denotes the variant [`label [of ty]]. 
           - [Reither (constr:bool, tys, _, _)]: [constr] denotes whether field is a constant (empty) constructor, [tys] is a list of [type_expr]
           - [Rabsent] is used for merging '&' constraints in poylmorphic viarants (hence we ignore it).
        *)
      let closed_flag = if row_closed then Closed else Open in
      let fields =
        row_fields
        |> List.filter_map ~f:(fun (label, row_field) ->
               let label = Located.mk label in
               match row_field with
               | Rpresent None -> Some (rtag label true [])
               | Rpresent (Some ty) -> Some (rtag label false [ pcore_type_of_ttype_expr ~loc ty ])
               | Reither (constr, tys, _, _) ->
                 Some (rtag label constr (List.map ~f:(pcore_type_of_ttype_expr ~loc) tys))
               | _ -> None)
      in
      ptyp_variant fields closed_flag None
    | Tpackage (path, lidents, tys) ->
      (* Package consists of a module path [path] and a list of type constraints, 
           defined by [lidents] and [tys] (newer compiler verisions 
           provide a zipped list). 
        
           Examples: (module S) or (module S with type t1 = T1 and ...)
        *)
      let lident_tys =
        List.map2_exn lidents tys ~f:(fun lident ty ->
            Located.mk lident, pcore_type_of_ttype_expr ~loc ty)
      in
      ptyp_package (Located.mk (lident_of_path path), lident_tys)
    | Tpoly (ty, tys) ->
      (* Polymorphic type (forall):

           [tys] is the list of type variables (hence should be [Tunivar]s) 
           and [ty] is the qualified type. 
           
           Example: ('a 'b 'c) ty
        *)
      let tvs =
        tys
        |> List.filter_map ~f:(fun ty ->
               match ty.desc with
               | Tunivar tv -> Option.(tv >>| Located.mk)
               | _ -> None)
      in
      ptyp_poly tvs (pcore_type_of_ttype_expr ~loc ty)
    | Tnil | Tfield _ | Tobject _ ->
      (* [Tnil], [Tfield] and [Tobject] are used for object types (not supported) *)
      raise_errorf ~loc "[%%open]: object types are not supported."
    | Tlink _ | Tsubst _ ->
      (* [Tlink] and [Tsubst] are used internally by the compiler. *)
      assert false


  let plabel_decl_of_tlabel_decl ~loc label =
    let (module B) = Ast_builder.make loc in
    let open B in
    let name = Located.mk (Ident.name label.ld_id) in
    label_declaration
      ~name
      ~mutable_:(Migrate.mutable_flag label.ld_mutable)
      ~type_:(pcore_type_of_ttype_expr ~loc label.ld_type)


  let pconstr_decl_of_tconstr_decl ~loc constr =
    let (module B) = Ast_builder.make loc in
    let open B in
    let name = Located.mk (Ident.name constr.cd_id) in
    let args =
      match constr.cd_args with
      | Cstr_tuple tys -> Pcstr_tuple (List.map tys ~f:(pcore_type_of_ttype_expr ~loc))
      | Cstr_record labels -> Pcstr_record (List.map labels ~f:(plabel_decl_of_tlabel_decl ~loc))
    in
    let res = Option.(constr.cd_res >>| pcore_type_of_ttype_expr ~loc) in
    constructor_declaration ~name ~args ~res


  let ptype_kind_of_ttype_decl_kind ~loc kind =
    let (module B) = Ast_builder.make loc in
    let open B in
    match kind with
    | Type_abstract -> Ptype_abstract
    | Type_open -> Ptype_open
    | Type_record (labels, _) -> Ptype_record (List.map labels ~f:(plabel_decl_of_tlabel_decl ~loc))
    | Type_variant constrs ->
      Ptype_variant (List.map constrs ~f:(pconstr_decl_of_tconstr_decl ~loc))


  let ptype_params_and_cstrs_of_ttype_params ~loc params =
    let (module B) = Ast_builder.make loc in
    let open B in
    let fresh_tvar =
      let i = ref 0 in
      fun () ->
        let tv = "a" ^ Int.to_string !i in
        Int.incr i;
        ptyp_var tv
    in
    let params, constraints =
      params
      |> List.map ~f:(fun param ->
             match param.desc with
             | Tvar _ -> (pcore_type_of_ttype_expr ~loc param, (NoVariance, NoInjectivity)), None
             | _ ->
               let tv = fresh_tvar () in
               (tv, (NoVariance, NoInjectivity)), Some (tv, pcore_type_of_ttype_expr ~loc param, loc))
      |> List.unzip
    in
    params, List.filter_opt constraints


  let open_ptype_decl_of_ttype_decl ~loc name ptype_lident ttype_decl =
    let (module B) = Ast_builder.make loc in
    let open B in
    match ttype_decl.type_private with
    | Private -> raise_errorf ~loc "[%%open]: cannot open a private type."
    | Public ->
      let name = Located.mk name in
      let params, cstrs = ptype_params_and_cstrs_of_ttype_params ~loc ttype_decl.type_params in
      let kind = ptype_kind_of_ttype_decl_kind ~loc ttype_decl.type_kind in
      let manifest = Some (ptyp_constr (Located.mk ptype_lident) (fst (List.unzip params))) in
      type_declaration ~name ~params ~cstrs ~kind ~manifest ~private_:Public


  let closed_ptype_decl_of_ttype_decl ~loc name ptype_lident ttype_decl =
    let (module B) = Ast_builder.make loc in
    let open B in
    let params, _ = ptype_params_and_cstrs_of_ttype_params ~loc ttype_decl.type_params in
    let manifest = Some (ptyp_constr (Located.mk ptype_lident) (fst (List.unzip params))) in
    type_declaration
      ~name:(Located.mk name)
      ~params
      ~kind:Ptype_abstract
      ~manifest
      ~private_:Public
      ~cstrs:[]


  let expand ~loc mod_ident { type_ident; type_alias; type_kind } =
    let (module B) = Ast_builder.make loc in
    let open B in
    let name = Option.value type_alias ~default:type_ident in
    let ttype_decl = find_type ~loc (Lazy.force_val env) mod_ident type_ident in
    let ptype_lident = Ldot (mod_ident, type_ident) in
    let ptype_decl =
      match type_kind with
      | Kind_closed -> closed_ptype_decl_of_ttype_decl ~loc name ptype_lident ttype_decl
      | Kind_open -> open_ptype_decl_of_ttype_decl ~loc name ptype_lident ttype_decl
    in
    pstr_type Nonrecursive [ ptype_decl ]
end

module Value = struct
  include Parsed.Value

  let expand mod_ident ~loc { val_ident; val_alias } =
    let ident = val_alias |> Option.value ~default:val_ident in
    let (module B) = Ast_builder.make loc in
    let open B in
    pstr_value
      Nonrecursive
      [ value_binding
          ~pat:(ppat_var (Located.mk ident))
          ~expr:(pexp_ident (Located.mk (Ldot (mod_ident, val_ident))))
      ]
end

module Item = struct
  include Parsed.Item

  let expand ~loc mod_ident item =
    match item with
    | Type t -> Type.expand ~loc mod_ident t
    | Value v -> Value.expand ~loc mod_ident v
    | Module m -> Module.expand ~loc mod_ident m
    | Module_type mty -> Module_type.expand ~loc mod_ident mty
end

module Payload = struct
  include Parsed.Payload

  let expand ~loc ~tool_name { open_mod_ident; open_items } =
    let (module B) = Ast_builder.make loc in
    let open B in
    if String.equal tool_name "ocamldep"
    then pstr_open (open_infos ~override:Fresh ~expr:(pmod_ident (Located.mk open_mod_ident)))
    else (
      let value_bindings = List.map ~f:(Item.expand ~loc open_mod_ident) open_items in
      pstr_open (open_infos ~override:Fresh ~expr:(pmod_structure value_bindings)))
end

let pattern =
  let open Ast_pattern in
  pstr (pstr_eval (estring __) nil ^:: nil)


let expand ~ctxt payload_string =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let payload =
    match Parse.payload (Lexing.from_string payload_string) with
    | Ok payload -> payload
    | Error message -> raise_errorf ~loc "%s" message
  in
  let tool_name = Expansion_context.Extension.tool_name ctxt in
  Payload.expand ~tool_name ~loc payload


let open_extension = Extension.V3.declare name Extension.Context.structure_item pattern expand
let () = Driver.register_transformation ~extensions:[ open_extension ] name
