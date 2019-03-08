open Ppxlib

let prefix ~type_name ?constructor_name () =
  let type_prefix = Util.affix_from_type_name ~kind:`Prefix type_name in
  let constructor_prefix =
    match constructor_name with
    | None -> ""
    | Some constructor_name -> (String.lowercase_ascii constructor_name) ^ "_"
  in
  Printf.sprintf "%s%s" type_prefix constructor_prefix

let factory_name prefix = Printf.sprintf "%sfactory" prefix

let _name_from_type_and_constructor_name ~type_name ~constructor_name =
  factory_name (prefix ~type_name ~constructor_name ())

let _name_from_type_name type_name =
  factory_name (prefix ~type_name ())

let arg_names_from_labels labels =
  List.map (fun {pld_name; _} -> pld_name.txt) labels

let arg_names_from_tuple types =
  List.mapi (fun i _ -> Printf.sprintf "tup%d" i) types

module Str = struct
  let factory_fun_expr ~loc ~return_expr ~arg_names ~defaults =
    List.fold_right2
      ( fun name default acc ->
          let arg_label = Optional name in
          let pattern = Ast_builder.Default.ppat_var ~loc {txt = name; loc} in
          Ast_builder.Default.pexp_fun ~loc arg_label default pattern acc
      )
      arg_names
      defaults
      [%expr fun () -> [%e return_expr]]

  let default_arg_from_core_type ~loc core_type =
    match core_type with
    | [%type: [%t? _] option] -> None
    | _ -> Some (Default.expr_from_core_type_exn ~loc core_type)

  let defaults_from_label_decl ~loc labels =
    List.map (fun {pld_type; _} -> default_arg_from_core_type ~loc pld_type) labels

  let fixed_field_binding ~loc name =
    let lident = {txt = Lident name; loc} in
    (lident, Util.Expr.var ~loc name)

  let fun_expr_from_labels ~loc ?constructor_name labels =
    let arg_names = arg_names_from_labels labels in
    let fields_bindings = List.map (fixed_field_binding ~loc) arg_names in
    let record_expr = Ast_builder.Default.pexp_record ~loc fields_bindings None in
    let return_expr =
      match constructor_name with
      | None -> record_expr
      | Some constructor_name -> Util.Expr.constructor ~loc ~constructor_name (Some record_expr)
    in
    let defaults = defaults_from_label_decl ~loc labels in
    factory_fun_expr ~loc ~return_expr ~arg_names ~defaults

  let value_binding ~loc ~factory_name ~expr =
    let pat = Ast_builder.Default.ppat_var ~loc {txt = factory_name; loc} in
    let value_binding = Ast_builder.Default.value_binding ~loc ~pat ~expr in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_binding]

  let from_labels ~loc ~factory_name ?constructor_name labels =
    let expr = fun_expr_from_labels ~loc ?constructor_name labels in
    value_binding ~loc ~factory_name ~expr

  let from_record ~loc ~type_name ~labels =
    let factory_name = _name_from_type_name type_name in
    [from_labels ~loc ~factory_name labels]

  let defaults_from_tuple ~loc types =
    List.map (fun core_type -> default_arg_from_core_type ~loc core_type) types

  let fun_expr_from_constructor_tuple ~loc ~constructor_name types =
    let arg_names = arg_names_from_tuple types in
    let tuple_bindings = List.map (Util.Expr.var ~loc) arg_names in
    let constructor_arg_expr =
      match tuple_bindings with
      | [] -> None
      | [expr] -> Some expr
      | _ -> Some (Ast_builder.Default.pexp_tuple ~loc tuple_bindings)
    in
    let return_expr = Util.Expr.constructor ~loc ~constructor_name constructor_arg_expr in
    let defaults = defaults_from_tuple ~loc types in
    factory_fun_expr ~loc ~return_expr ~arg_names ~defaults

  let from_constructor_tuple ~loc ~factory_name ~constructor_name types =
    let expr = fun_expr_from_constructor_tuple ~loc ~constructor_name types in
    value_binding ~loc ~factory_name ~expr

  let from_constructor_record ~loc ~factory_name ~constructor_name labels =
    from_labels ~loc ~factory_name ~constructor_name labels

  let from_constructor ~loc ~type_name {pcd_name = {txt = constructor_name; _}; pcd_args; _} =
    let factory_name = _name_from_type_and_constructor_name ~type_name ~constructor_name in
    match pcd_args with
    | Pcstr_tuple types -> from_constructor_tuple ~loc ~factory_name ~constructor_name types
    | Pcstr_record labels -> from_constructor_record ~loc ~factory_name ~constructor_name labels

  let from_td ~loc {ptype_name = {txt = type_name; _}; ptype_kind; _} =
    match ptype_kind with
    | Ptype_record labels -> from_record ~loc ~type_name ~labels
    | Ptype_variant constructors -> List.map (from_constructor ~loc ~type_name) constructors
    | Ptype_abstract -> []
    | Ptype_open -> Raise.Factory.unhandled_type_kind ~loc "open"

  let from_type_decl ~loc ~path:_ (_rec_flag, tds) =
    List.flatten @@ List.map (from_td ~loc) tds
end

module Sig = struct
  let factory_fun_val ~loc ~return_type ~arg_names ~arg_types =
    List.fold_right2
      ( fun name typ acc ->
          let arg_label = Optional name in
          Ast_builder.Default.ptyp_arrow ~loc arg_label typ acc
      )
      arg_names
      arg_types
      [%type: unit -> [%t return_type]]

  let arg_type_from_core_type core_type =
    match core_type with
    | [%type: [%t? a] option] -> a
    | _ -> core_type

  let arg_types_from_labels labels =
    List.map (fun {pld_type = typ; _} -> arg_type_from_core_type typ) labels

  let fun_val_from_labels ~loc ~return_type labels =
    let arg_names = arg_names_from_labels labels in
    let arg_types = arg_types_from_labels labels in
    factory_fun_val ~loc ~return_type ~arg_names ~arg_types

  let fun_val_from_constructor_tuple ~loc ~return_type types =
    let arg_names = arg_names_from_tuple types in
    let arg_types = List.map arg_type_from_core_type types in
    factory_fun_val ~loc ~return_type ~arg_names ~arg_types

  let value_descr ~loc ~factory_name ~type_ =
    let name = {txt = factory_name; loc} in
    let value_description = Ast_builder.Default.value_description ~loc ~name ~type_ ~prim:[] in
    Ast_builder.Default.psig_value ~loc value_description

  let from_labels ~loc ~factory_name ~return_type labels =
    let type_ = fun_val_from_labels ~loc ~return_type labels in
    value_descr ~loc ~factory_name ~type_

  let from_constructor_tuple ~loc ~factory_name ~return_type types =
    let type_ = fun_val_from_constructor_tuple ~loc ~return_type types in
    value_descr ~loc ~factory_name ~type_

  let from_record ~loc ~type_name ~return_type ~labels =
    let factory_name = _name_from_type_name type_name in
    [from_labels ~loc ~factory_name ~return_type labels]

  let from_constructor ~loc ~type_name ~return_type {pcd_name; pcd_args; _} =
    let {txt = constructor_name; _} = pcd_name in
    let factory_name = _name_from_type_and_constructor_name ~type_name ~constructor_name in
    match pcd_args with
    | Pcstr_tuple types -> from_constructor_tuple ~loc ~factory_name ~return_type types
    | Pcstr_record labels -> from_labels ~loc ~factory_name ~return_type labels

  let from_td ~loc ({ptype_name = {txt = type_name; _}; ptype_kind; _} as td) =
    let return_type = Util.core_type_from_type_decl ~loc td in
    match ptype_kind with
    | Ptype_record labels -> from_record ~loc ~type_name ~return_type ~labels
    | Ptype_variant ctors -> List.map (from_constructor ~loc ~type_name ~return_type) ctors
    | Ptype_abstract -> []
    | Ptype_open -> Raise.Factory.unhandled_type_kind ~loc "open"

  let from_type_decl ~loc ~path:_ (_rec_flag, tds) =
    List.flatten @@ List.map (from_td ~loc) tds
end

let from_str_type_decl =
  Deriving.Generator.make_noarg Str.from_type_decl

let from_sig_type_decl =
  Deriving.Generator.make_noarg Sig.from_type_decl
