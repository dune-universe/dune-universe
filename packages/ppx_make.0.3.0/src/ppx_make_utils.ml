module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper

(* Core Type Utils *)

let unit_core_type ~loc =
  Ast_helper.Typ.constr ~loc P.{ txt = P.Lident "unit"; loc } []

let core_type_of_name ?(params = []) P.{ txt = name; loc } =
  Ast_helper.Typ.constr ~loc P.{ txt = P.Lident name; loc } params

let is_core_type_option (ct : P.core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, _) -> true
  | _ -> false

let is_core_type_list (ct : P.core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "list"; _ }, _) -> true
  | _ -> false

let is_core_type_string (ct : P.core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> true
  | _ -> false

let is_core_type_optional (ct : P.core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, _)
  | Ptyp_constr ({ txt = Lident "list"; _ }, _)
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
      true
  | _ -> false

let strip_option (ct : P.core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) -> in_ct
  | _ -> ct

let default_expression_of_core_type ~loc (ct : P.core_type) =
  let open P in
  if is_core_type_list ct then
    Some [%expr []]
  else if is_core_type_string ct then
    Some [%expr ""]
  else
    None

(* Attributes Utils *)

type attr_type = No_attr | Main | Required | Default of P.expression

let get_attributes (attrs : P.attribute list) =
  let check_res ~loc acc cur =
    match (acc, cur) with
    | No_attr, _ -> cur
    | _, No_attr -> acc
    | _, _ ->
        P.Location.raise_errorf ~loc
          "single field cannot have more than one attributes"
  in
  List.fold_left
    (fun acc (attr : P.attribute) ->
      (match attr.attr_name.txt with
      | "main" -> Main
      | "required" -> Required
      | "default" -> (
          match attr.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Default expr
          | _ ->
              P.Location.raise_errorf ~loc:attr.attr_loc
                "value in default attribute is not supported")
      (* ignore unknown attrs *)
      | _ -> No_attr)
      |> check_res ~loc:attr.attr_loc acc)
    No_attr attrs

(* Misc. Utils *)

let unsupported_error P.{ txt; loc } =
  P.Location.raise_errorf ~loc "type %s cannot be derived" txt

let make_type_decl_generator f =
  P.Deriving.Generator.V2.make_noarg (fun ~ctxt (rec_flag, tds) ->
      let loc = P.Expansion_context.Deriver.derived_item_loc ctxt in
      tds |> List.map (f ~loc rec_flag) |> List.concat)

let gen_make_name P.{ txt = name; loc } = P.{ txt = "make_" ^ name; loc }

let gen_make_choice_name P.{ txt = name; _ } P.{ txt = choice_name; loc } =
  let txt = String.lowercase_ascii ("make_" ^ choice_name ^ "_of_" ^ name) in
  P.{ txt; loc }

let gen_tuple_label_string index = "v" ^ string_of_int index

let longident_loc_of_name P.{ txt; loc } = P.{ txt = P.Lident txt; loc }

let add_choice_to_expr choice expr =
  match choice with
  | Some choice_name ->
      let lid = longident_loc_of_name choice_name in
      Ast_helper.Exp.construct lid (Some expr)
  | None -> expr

let params_core_type_of_type_decl ~loc:_ (td : P.type_declaration) =
  List.map (fun (ct, _) -> ct) td.ptype_params
