module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper
module Utils = Ppx_make_utils

(* generate function expression for option *)
let fun_expression_of_option ~loc (ct : P.core_type) : P.expression =
  Ast_helper.with_default_loc loc (fun () ->
      let open P in
      match Utils.get_attributes ct.ptyp_attributes with
      | Default e -> [%expr fun ?(value = [%e e]) () -> Some value]
      | No_attr -> [%expr fun ?value () -> value]
      | _ ->
          P.Location.raise_errorf ~loc
            "option types only support `default` attribute")

(* generate function expression for record *)
let fun_expression_of_record ~loc ?choice (lds : P.label_declaration list) :
    P.expression =
  Ast_helper.with_default_loc loc (fun () ->
      let attrs : Utils.attr_type list =
        List.map (fun ld -> Utils.get_attributes ld.P.pld_attributes) lds
      in
      let label_pats, main_pats =
        List.fold_left2
          (fun (label_pats, main_pats) P.{ pld_name; pld_type; pld_loc; _ }
               (attr : Utils.attr_type) ->
            let loc = pld_loc in
            Ast_helper.with_default_loc loc (fun () ->
                let pat = Ast_helper.Pat.var pld_name in
                let optional = Utils.is_core_type_optional pld_type in
                match (attr, optional) with
                | Main, _ -> (label_pats, pat :: main_pats)
                | Default def, _ ->
                    let arg_label = P.Optional pld_name.txt in
                    ((arg_label, Some def, pat) :: label_pats, main_pats)
                | No_attr, true ->
                    let arg_label = P.Optional pld_name.txt in
                    let def =
                      Utils.default_expression_of_core_type ~loc pld_type
                    in
                    ((arg_label, def, pat) :: label_pats, main_pats)
                | _, _ ->
                    let arg_label = P.Labelled pld_name.txt in
                    ((arg_label, None, pat) :: label_pats, main_pats)))
          ([], []) lds attrs
      in
      let main_pats =
        if main_pats = [] then [ Ast_helper.Pat.any () ] else main_pats
      in
      lds
      |> List.map2
           (fun attr P.{ pld_name; pld_type; pld_loc; _ } ->
             let loc = pld_loc in
             Ast_helper.with_default_loc loc (fun () ->
                 let option_ = Utils.is_core_type_option pld_type in
                 let lid = Utils.longident_loc_of_name pld_name in
                 let expr = Ast_helper.Exp.ident lid in
                 match (attr, option_) with
                 | Utils.Default _, true ->
                     let open P in
                     (lid, [%expr Some [%e expr]])
                 | _, _ -> (lid, expr)))
           attrs
      |> (fun labels -> Ast_helper.Exp.record labels None)
      |> Utils.add_choice_to_expr choice
      |> fun expr ->
      List.fold_left
        (fun acc cur_pat -> Ast_helper.Exp.fun_ P.Nolabel None cur_pat acc)
        expr main_pats
      |> fun expr ->
      List.fold_left
        (fun acc (arg_label, default_expr, cur_pat) ->
          Ast_helper.Exp.fun_ arg_label default_expr cur_pat acc)
        expr label_pats)

(* generate function expression for tuple *)
let fun_expression_of_tuple ~loc ?choice (cts : P.core_type list) : P.expression
    =
  Ast_helper.with_default_loc loc (fun () ->
      let cts_attrs =
        cts
        |> List.map (fun ct -> Utils.get_attributes ct.P.ptyp_attributes)
        |> List.combine cts
      in
      let label_pats =
        cts_attrs
        |> List.mapi
             (fun index ((ct : P.core_type), (attr : Utils.attr_type)) ->
               let loc = ct.ptyp_loc in
               Ast_helper.with_default_loc loc (fun () ->
                   let label_name = Utils.gen_tuple_label_string index in
                   let pat = Ast_helper.Pat.var { txt = label_name; loc } in
                   let optional = Utils.is_core_type_optional ct in
                   match (attr, optional) with
                   | Main, _ ->
                       P.Location.raise_errorf ~loc:ct.ptyp_loc
                         "tuple types do not support `main` attribute"
                   | Default def, _ -> (P.Optional label_name, Some def, pat)
                   | No_attr, true ->
                       let def =
                         Utils.default_expression_of_core_type ~loc ct
                       in
                       (P.Optional label_name, def, pat)
                   | _, _ -> (P.Labelled label_name, None, pat)))
        |> List.rev
      in
      cts_attrs
      |> List.mapi (fun index ((ct : P.core_type), (attr : Utils.attr_type)) ->
             let loc = ct.ptyp_loc in
             Ast_helper.with_default_loc loc (fun () ->
                 let option_ = Utils.is_core_type_option ct in
                 let lid =
                   Utils.longident_loc_of_name
                     P.{ txt = Utils.gen_tuple_label_string index; loc }
                 in
                 let expr = Ast_helper.Exp.ident lid in
                 match (attr, option_) with
                 | Utils.Default _, true ->
                     let open P in
                     [%expr Some [%e expr]]
                 | _, _ -> expr))
      |> (fun exprs ->
           match exprs with [ expr ] -> expr | _ -> Ast_helper.Exp.tuple exprs)
      |> Utils.add_choice_to_expr choice
      |> Ast_helper.Exp.fun_ P.Nolabel None (Ast_helper.Pat.any ())
      |> fun expr ->
      List.fold_left
        (fun acc (arg_label, default_expr, cur_pat) ->
          Ast_helper.Exp.fun_ arg_label default_expr cur_pat acc)
        expr label_pats)

(* generate function core type for option *)
let fun_core_type_of_option ~loc (name, params) (in_ct : P.core_type) :
    P.core_type =
  Ast_helper.with_default_loc loc (fun () ->
      let return_ct = Utils.core_type_of_name ~params name in
      let open P in
      [%type: ?value:[%t in_ct] -> unit -> [%t return_ct]])

(* generate function core type for record *)
let fun_core_type_of_record ~loc (name, params) (lds : P.label_declaration list)
    : P.core_type =
  Ast_helper.with_default_loc loc (fun () ->
      let label_cts, main_cts =
        List.fold_left
          (fun (label_cts, main_cts) P.{ pld_name; pld_type; pld_attributes; _ } ->
            let attr : Utils.attr_type = Utils.get_attributes pld_attributes in
            let optional = Utils.is_core_type_optional pld_type in
            match (attr, optional) with
            | Main, _ -> (label_cts, pld_type :: main_cts)
            | Default _, _ | No_attr, true ->
                let ct = Utils.strip_option pld_type in
                ((P.Optional pld_name.txt, ct) :: label_cts, main_cts)
            | _, _ ->
                ((P.Labelled pld_name.txt, pld_type) :: label_cts, main_cts))
          ([], []) lds
      in
      let main_cts =
        if main_cts = [] then [ Utils.unit_core_type ~loc ] else main_cts
      in
      name
      |> Utils.core_type_of_name ~params
      |> fun ct ->
      List.fold_left
        (fun acc cur -> Ast_helper.Typ.arrow P.Nolabel cur acc)
        ct main_cts
      |> fun ct ->
      List.fold_left
        (fun acc (arg_label, cur) -> Ast_helper.Typ.arrow arg_label cur acc)
        ct label_cts)

(* generate function core type for tuple *)
let fun_core_type_of_tuple ~loc (name, params) (cts : P.core_type list) :
    P.core_type =
  Ast_helper.with_default_loc loc (fun () ->
      let label_cts =
        cts
        |> List.mapi (fun index (ct : P.core_type) ->
               let attr : Utils.attr_type =
                 Utils.get_attributes ct.ptyp_attributes
               in
               let optional = Utils.is_core_type_optional ct in
               let label_name = Utils.gen_tuple_label_string index in
               match (attr, optional) with
               | Main, _ ->
                   P.Location.raise_errorf ~loc:ct.ptyp_loc
                     "tuple types do not support `main` attribute"
               | Default _, _ | No_attr, true ->
                   let ct = Utils.strip_option ct in
                   (P.Optional label_name, ct)
               | _, _ -> (P.Labelled label_name, ct))
        |> List.rev
      in

      name
      |> Utils.core_type_of_name ~params
      |> Ast_helper.Typ.arrow P.Nolabel (Utils.unit_core_type ~loc)
      |> fun ct ->
      List.fold_left
        (fun acc (arg_label, cur) -> Ast_helper.Typ.arrow arg_label cur acc)
        ct label_cts)

(* generate structure item for core type *)
let str_item_of_core_type (name, params) (ct : P.core_type) : P.structure_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      let pat = Ast_helper.Pat.var @@ Utils.gen_make_name name in
      let fun_ct, expr =
        match ct.ptyp_desc with
        | Ptyp_tuple cts ->
            (* T1 * ... * Tn *)
            ( fun_core_type_of_tuple ~loc (name, params) cts,
              fun_expression_of_tuple ~loc cts )
        | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
            (* T option *)
            ( fun_core_type_of_option ~loc (name, params) in_ct,
              fun_expression_of_option ~loc ct )
        | _ -> Utils.unsupported_error name
      in
      let open P in
      [%stri let ([%p pat] : [%t fun_ct]) = [%e expr]])

(* generate structure item for record *)
let str_item_of_record ~loc (name, params) (lds : P.label_declaration list) :
    P.structure_item =
  Ast_helper.with_default_loc loc (fun () ->
      let pat = Ast_helper.Pat.var @@ Utils.gen_make_name name in
      let ct = fun_core_type_of_record ~loc (name, params) lds in
      let expr = fun_expression_of_record ~loc lds in
      let open P in
      [%stri let ([%p pat] : [%t ct]) = [%e expr]])

(* generate structure item for a choice in variant *)
let str_item_of_variant_choice (name, params) (cd : P.constructor_declaration) :
    P.structure_item =
  let loc = cd.pcd_loc in
  Ast_helper.with_default_loc loc (fun () ->
      if cd.pcd_res != None then
        Utils.unsupported_error name
      else
        let pat =
          Ast_helper.Pat.var @@ Utils.gen_make_choice_name name cd.pcd_name
        in
        let ct, expr =
          match cd.pcd_args with
          | Pcstr_tuple [] ->
              let lid = Utils.longident_loc_of_name cd.pcd_name in
              let exp = Ast_helper.Exp.construct lid None in
              ( fun_core_type_of_tuple ~loc (name, params) [],
                let open P in
                [%expr fun () -> [%e exp]] )
          | Pcstr_tuple cts ->
              ( fun_core_type_of_tuple ~loc (name, params) cts,
                fun_expression_of_tuple ~loc ~choice:cd.pcd_name cts )
          | Pcstr_record lds ->
              ( fun_core_type_of_record ~loc (name, params) lds,
                fun_expression_of_record ~loc ~choice:cd.pcd_name lds )
        in
        let open P in
        [%stri let ([%p pat] : [%t ct]) = [%e expr]])

(* generate signature item for core type *)
let sig_item_of_core_type (name, params) (ct : P.core_type) : P.signature_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      let fun_name = Utils.gen_make_name name in
      (match ct.ptyp_desc with
      | Ptyp_tuple cts ->
          (* T1 * ... * Tn *)
          fun_core_type_of_tuple ~loc (name, params) cts
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
          (* T option *)
          fun_core_type_of_option ~loc (name, params) in_ct
      | _ -> Utils.unsupported_error name)
      |> Ast_helper.Val.mk fun_name
      |> Ast_helper.Sig.value)

(* generate signature item for record *)
let sig_item_of_record ~loc (name, params) (lds : P.label_declaration list) :
    P.signature_item =
  Ast_helper.with_default_loc loc (fun () ->
      let fun_name = Utils.gen_make_name name in
      lds
      |> fun_core_type_of_record ~loc (name, params)
      |> Ast_helper.Val.mk fun_name
      |> Ast_helper.Sig.value)

(* generate signature item for a choice of variant *)
let sig_item_of_variant_choice (name, params) (cd : P.constructor_declaration) :
    P.signature_item =
  let loc = cd.pcd_loc in
  Ast_helper.with_default_loc loc (fun () ->
      if cd.pcd_res != None then
        Utils.unsupported_error name
      else
        let fun_name = Utils.gen_make_choice_name name cd.pcd_name in
        (match cd.pcd_args with
        | Pcstr_tuple cts -> fun_core_type_of_tuple ~loc (name, params) cts
        | Pcstr_record lds -> fun_core_type_of_record ~loc (name, params) lds)
        |> Ast_helper.Val.mk fun_name
        |> Ast_helper.Sig.value)

(* generate structure for type declaration *)
let structure_of_type_decl
    ~loc:_
    (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.structure =
  let loc = td.ptype_loc in
  let name = td.ptype_name in
  let params = Utils.params_core_type_of_type_decl ~loc td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      [ str_item_of_core_type (name, params) ct ]
  | { ptype_kind = Ptype_variant cds; _ } ->
      (* type t = C of T | ... *)
      List.map (str_item_of_variant_choice (name, params)) cds
  | { ptype_kind = Ptype_record lds; _ } ->
      (* type t = {l: T; ...} *)
      [ str_item_of_record ~loc (name, params) lds ]
  | _ -> Utils.unsupported_error name

(* generate signature for type declaration *)
let signature_of_type_decl
    ~loc:_
    (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.signature =
  let loc = td.ptype_loc in
  let name = td.ptype_name in
  let params = Utils.params_core_type_of_type_decl ~loc td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      [ sig_item_of_core_type (name, params) ct ]
  | { ptype_kind = Ptype_variant cds; _ } ->
      (* type t = C of T | ... *)
      List.map (sig_item_of_variant_choice (name, params)) cds
  | { ptype_kind = Ptype_record lds; _ } ->
      (* type t = {l: T; ...} *)
      [ sig_item_of_record ~loc (name, params) lds ]
  | _ -> Utils.unsupported_error name

let str_type_decl = Utils.make_type_decl_generator structure_of_type_decl

let sig_type_decl = Utils.make_type_decl_generator signature_of_type_decl

let deriver = P.Deriving.add "make" ~str_type_decl ~sig_type_decl
