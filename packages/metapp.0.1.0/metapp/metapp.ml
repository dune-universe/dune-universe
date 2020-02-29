include Metapp_preutils.Header
include Metapp_preutils.Footer

(** {1 General purpose functions} *)

let rec extract_first (p : 'a -> 'b option) (l : 'a list)
    : ('b * 'a list) option =
  match l with
  | [] -> None
  | hd :: tl ->
      match p hd with
      | Some b -> Some (b, tl)
      | None ->
          match extract_first p tl with
          | Some (b, tl) -> Some (b, hd :: tl)
          | None -> None

(** {1 Attribute management} *)

module Attr = struct
  let mk (name : Ast_helper.str) (payload : Parsetree.payload) =
    [%meta if Sys.ocaml_version < "4.08.0" then
      [%e (name, payload)]
    else
      [%e Ast_helper.Attr.mk name payload]]

  let name (attribute : Parsetree.attribute) : Ast_helper.str =
    [%meta if Sys.ocaml_version < "4.08.0" then
      [%e fst attribute]
    else
      [%e attribute.attr_name]]

  let payload (attribute : Parsetree.attribute) : Parsetree.payload =
    [%meta if Sys.ocaml_version < "4.08.0" then
      [%e snd attribute]
    else
      [%e attribute.attr_payload]]

  let to_loc (attribute : Parsetree.attribute) : Location.t =
    [%meta if Sys.ocaml_version < "4.08.0" then
      [%e (fst attribute).loc]
    else
      [%e attribute.attr_loc]]

  let find (attr_name : string) (attributes : Parsetree.attributes)
      : Parsetree.attribute option =
    List.find_opt (fun attribute ->
      String.equal (name attribute).txt attr_name) attributes

  let chop (attr_name : string) (attributes : Parsetree.attributes)
      : (Parsetree.attribute * Parsetree.attributes) option =
    extract_first (fun attribute ->
      if String.equal (name attribute).txt attr_name then
        Some attribute
      else
        None) attributes
end

(** {1 Module binding and declaration} *)

[%%meta if Sys.ocaml_version < "4.10.0" then [%stri
let get_mod_name mod_ name =
  match name with
  | None ->
      invalid_arg (Printf.sprintf
        "%s.mk: anonymous modules are not supported with OCaml <4.10.0" mod_)
  | Some name -> name]
else Metapp_preutils.Stri.of_list []]

module Md = struct
  let mk (mod_name : string option Location.loc)
      (s : Parsetree.module_type) : Parsetree.module_declaration =
    [%meta if Sys.ocaml_version < "4.10.0" then
      [%e Ast_helper.Md.mk (map_loc (get_mod_name "Md") mod_name) s]
    else
      [%e Ast_helper.Md.mk mod_name s]]
end

module Mb = struct
  let mk (mod_name : string option Location.loc)
      (s : Parsetree.module_expr) : Parsetree.module_binding =
    [%meta if Sys.ocaml_version < "4.10.0" then
      [%e Ast_helper.Mb.mk (map_loc (get_mod_name "Mb") mod_name) s]
    else
      [%e Ast_helper.Mb.mk mod_name s]]
end

(** {1 Expressions} *)

module Exp = struct
  include Metapp_preutils.Exp

  let send ?loc ?attrs (expression : Parsetree.expression)
      (str : Ast_helper.str) : Parsetree.expression =
    [%meta if Sys.ocaml_version >= "4.05.0" then [%e
      Ast_helper.Exp.send ?loc ?attrs expression str]
    else [%e
      Ast_helper.Exp.send ?loc ?attrs expression str.txt]]

  let newtype ?loc ?attrs (name : Ast_helper.str) (ty : Parsetree.expression)
      : Parsetree.expression =
    [%meta if Sys.ocaml_version >= "4.05.0" then [%e
      Ast_helper.Exp.newtype ?loc ?attrs name ty]
    else [%e
      Ast_helper.Exp.newtype ?loc ?attrs name.txt ty]]
end

(** {1 Mapper for [[@if bool]] notation} *)

let filter : Ast_mapper.mapper =
  let check_attr (attributes : Parsetree.attributes) =
    match Attr.find "if" attributes with
    | None -> true
    | Some attr -> bool_of_payload (Attr.payload attr) in
  let rec check_pat (p : Parsetree.pattern) =
    begin match p.ppat_desc with
    | Ppat_constraint (p, _) -> check_pat p
    | _ -> false
    end ||
    check_attr p.ppat_attributes in
  let check_value_binding (binding : Parsetree.value_binding) =
    check_attr binding.pvb_attributes in
  let check_value_description (description : Parsetree.value_description) =
    check_attr description.pval_attributes in
  let check_case (case : Parsetree.case) =
    check_pat case.pc_lhs in
  let check_expr (e : Parsetree.expression) =
    check_attr e.pexp_attributes in
  let check_pat_snd (type a) (arg : a * Parsetree.pattern) =
    check_pat (snd arg) in
  let check_expr_snd (type a) (arg : a * Parsetree.expression) =
    check_expr (snd arg) in
  let check_type_declaration (declaration : Parsetree.type_declaration) =
    check_attr declaration.ptype_attributes in
  let pat (mapper : Ast_mapper.mapper) (p : Parsetree.pattern)
      : Parsetree.pattern =
    let p = Ast_mapper.default_mapper.pat mapper p in
    match p.ppat_desc with
    | Ppat_tuple args ->
        begin match List.filter check_pat args with
        | [] -> Pat.of_unit ()
        | [singleton] -> singleton
        | args -> { p with ppat_desc = Ppat_tuple args }
        end
    | Ppat_construct (lid, Some arg) ->
        if check_pat arg then
          p
        else
          { p with ppat_desc = Ppat_construct (lid, None)}
    | Ppat_variant (label, Some arg) ->
        if check_pat arg then
          p
        else
          { p with ppat_desc = Ppat_variant (label, None)}
    | Ppat_record (fields, closed_flag) ->
        begin match List.filter check_pat_snd fields with
        | [] -> { p with ppat_desc = Ppat_any }
        | fields -> { p with ppat_desc = Ppat_record (fields, closed_flag)}
        end
    | Ppat_array args ->
        { p with ppat_desc = Ppat_array (List.filter check_pat args)}
    | Ppat_or (a, b) when not (check_pat a) -> b
    | Ppat_or (a, b) when not (check_pat b) -> a
    | _ -> p in
  let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression)
      : Parsetree.expression =
    Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
    let e = Ast_mapper.default_mapper.expr mapper e in
    match e.pexp_desc with
    | Pexp_let (rec_flag, bindings, body) ->
        begin match List.filter check_value_binding bindings with
        | [] -> body
        | bindings -> { e with pexp_desc = Pexp_let (rec_flag, bindings, body) }
        end
    | Pexp_fun (_label, _default, pat, body) when not (check_pat pat) ->
        body
    | Pexp_function cases ->
        { e with pexp_desc = Pexp_function (List.filter check_case cases)}
    | Pexp_apply (f, args) ->
        let items =
          List.filter check_expr_snd ((Asttypes.Nolabel, f) :: args) in
        begin match
          extract_first (function (Asttypes.Nolabel, f) -> Some f | _ -> None)
            items
        with
        | None ->
            Location.raise_errorf ~loc:!Ast_helper.default_loc
              "No function left in this application"
        | Some (e, []) -> e
        | Some (f, args) ->
            { e with pexp_desc = Pexp_apply (f, args)}
        end
    | Pexp_match (e, cases) ->
        { e with pexp_desc = Pexp_match (e, List.filter check_case cases)}
    | Pexp_try (e, cases) ->
        { e with pexp_desc = Pexp_try (e, List.filter check_case cases)}
    | Pexp_tuple args ->
        begin match List.filter check_expr args with
        | [] -> Exp.of_unit ()
        | [singleton] -> singleton
        | args -> { e with pexp_desc = Pexp_tuple args }
        end
    | Pexp_construct (lid, Some arg) ->
        if check_expr arg then
          e
        else
          { e with pexp_desc = Pexp_construct (lid, None)}
    | Pexp_variant (label, Some arg) ->
        if check_expr arg then
          e
        else
          { e with pexp_desc = Pexp_variant (label, None)}
    | Pexp_record (fields, base) ->
        let base =
          match base with
          | Some expr when check_expr expr -> base
          | _ -> None in
        let fields = List.filter check_expr_snd fields in
        if fields = [] then
          Location.raise_errorf ~loc:!Ast_helper.default_loc
            "Cannot construct an empty record";
        { e with pexp_desc = Pexp_record (fields, base)}
    | Pexp_array args ->
        { e with pexp_desc = Pexp_array (List.filter check_expr args)}
    | Pexp_sequence (a, b) when not (check_expr a) -> b
    | Pexp_sequence (a, b) when not (check_expr b) -> a
    | _ -> e in
  let structure_item (mapper : Ast_mapper.mapper)
      (item : Parsetree.structure_item) : Parsetree.structure_item =
    let item = Ast_mapper.default_mapper.structure_item mapper item in
    match item.pstr_desc with
    | Pstr_value (rec_flag, bindings) ->
        begin match List.filter check_value_binding bindings with
        | [] -> Stri.of_list []
        | bindings -> { item with pstr_desc = Pstr_value (rec_flag, bindings)}
        end
    | Pstr_primitive description
      when not (check_value_description description) ->
        Stri.of_list []
    | Pstr_type (rec_flag, declarations) ->
        { item with pstr_desc =
          Pstr_type (rec_flag, List.filter check_type_declaration declarations)}
    | _ -> item in
  let signature_item (mapper : Ast_mapper.mapper)
      (item : Parsetree.signature_item) : Parsetree.signature_item =
    let item = Ast_mapper.default_mapper.signature_item mapper item in
    match item.psig_desc with
    | Psig_value description  when not (check_value_description description) ->
        Sigi.of_list []
    | Psig_type (rec_flag, declarations) ->
        { item with psig_desc =
          Psig_type (rec_flag, List.filter check_type_declaration declarations)}
    | _ -> item in
  { Ast_mapper.default_mapper with pat; expr; structure_item; signature_item }

(** {1 Signature type destruction} *)

[%%meta Metapp_preutils.Stri.of_list (
  if Sys.ocaml_version >= "4.08.0" then [%str
type sig_type = {
    id : Ident.t;
    decl : Types.type_declaration;
    rec_status : Types.rec_status;
    visibility : Types.visibility;
  }

let destruct_sig_type (item : Types.signature_item) : sig_type option =
  match item with
  | Sig_type (id, decl, rec_status, visibility) ->
      Some { id; decl; rec_status; visibility }
  | _ -> None]
else [%str
type sig_type = {
    id : Ident.t;
    decl : Types.type_declaration;
    rec_status : Types.rec_status;
  }

let destruct_sig_type (item : Types.signature_item) : sig_type option =
  match item with
  | Sig_type (id, decl, rec_status ) ->
      Some { id; decl; rec_status }
  | _ -> None])]

(** {1 Type construction} *)

module Typ = struct
  include Metapp_preutils.Typ

  let poly (names : Ast_helper.str list) (ty : Parsetree.core_type)
      : Parsetree.core_type =
    let names =
      [%meta if Sys.ocaml_version >= "4.05.0" then [%e
        names]
      else [%e
        List.map (fun (name : Ast_helper.str) -> name.txt) names]] in
    Ast_helper.Typ.poly names ty

  let poly_name name =
    [%meta if Sys.ocaml_version >= "4.05.0" then [%e
      (name : Ast_helper.str).txt]
    else [%e
      name]]
end

(** {1 Row fields} *)

module Rf = struct
  [%%meta if Sys.ocaml_version >= "4.08.0" then [%stri
    type desc = Parsetree.row_field_desc =
      | Rtag of Asttypes.label Location.loc * bool * Parsetree.core_type list
      | Rinherit of Parsetree.core_type]
  else [%stri
    type desc =
      | Rtag of Asttypes.label Location.loc * bool * Parsetree.core_type list
      | Rinherit of Parsetree.core_type]]

  let to_loc (_rf : Parsetree.row_field) : Location.t =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      _rf.prf_loc]
    else if Sys.ocaml_version >= "4.06.0" then [%e
      match _rf with
      | Rtag (label, _, _, _) -> label.loc
      | Rinherit _ -> !Ast_helper.default_loc]
    else [%e
      !Ast_helper.default_loc]]

  let to_attributes (rf : Parsetree.row_field) : Parsetree.attributes =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      rf.prf_attributes]
    else [%e
      match rf with
      | Rtag (_, attributes, _, _) -> attributes
      | Rinherit _ -> []]]

  let destruct (rf : Parsetree.row_field) : desc =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      rf.prf_desc]
    else [%e
      match rf with
      | Rtag (label, _, has_constant, args) ->
          let label =
            [%meta if Sys.ocaml_version >= "4.06.0" then [%e
              label]
            else [%e
              mkloc label]] in
          Rtag (label, has_constant, args)
      | Rinherit ty -> Rinherit ty]]

  let tag ?loc:_loc ?attrs (label : Asttypes.label Location.loc)
      (has_constant : bool) (args : Parsetree.core_type list)
      : Parsetree.row_field =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      Ast_helper.Rf.tag ?loc:_loc ?attrs label has_constant args]
    else [%e
      let label =
        [%meta if Sys.ocaml_version >= "4.06.0" then [%e
          label]
        else [%e
          label.txt]] in
      Rtag (label, Option.value ~default:[] attrs, has_constant,
        args)]]

  let inherit_ ?loc:_loc ?attrs:_attrs (core_type : Parsetree.core_type)
      : Parsetree.row_field =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      Ast_helper.Rf.mk ?loc:_loc ?attrs:_attrs (Rinherit core_type)]
    else [%e
      Rinherit core_type]]
end

(** {1 Object fields} *)

module Of = struct
  [%%meta if Sys.ocaml_version >= "4.06.0" then [%stri
    type t = Parsetree.object_field]
  else if Sys.ocaml_version >= "4.05.0" then [%stri
    type t =
      Asttypes.label Location.loc * Parsetree.attributes * Parsetree.core_type]
  else [%stri
    type t = Asttypes.label * Parsetree.attributes * Parsetree.core_type]]

  [%%meta if Sys.ocaml_version >= "4.08.0" then [%stri
    type desc = Parsetree.object_field_desc =
      | Otag of Asttypes.label Location.loc * Parsetree.core_type
      | Oinherit of Parsetree.core_type]
  else [%stri
    type desc =
      | Otag of Asttypes.label Location.loc * Parsetree.core_type
      | Oinherit of Parsetree.core_type]]

  let to_loc (_of : t) : Location.t =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      _of.pof_loc]
    else if Sys.ocaml_version >= "4.06.0" then [%e
      match _of with
      | Otag (label, _, _) -> label.loc
      | Oinherit _ -> !Ast_helper.default_loc]
    else if Sys.ocaml_version >= "4.05.0" then [%e
      let (label, _, _) = _of in
      label.loc]
    else [%e
      !Ast_helper.default_loc]]

  let to_attributes (of_ : t) : Parsetree.attributes =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      of_.pof_attributes]
    else if Sys.ocaml_version >= "4.06.0" then [%e
      match of_ with
      | Otag (_, attributes, _) -> attributes
      | Oinherit _ -> []]
    else [%e
      let (_, attributes, _) = of_ in
      attributes]]

  let destruct (of_ : t) : desc =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      of_.pof_desc]
    else if Sys.ocaml_version >= "4.06.0" then [%e
      match of_ with
      | Otag (label, _, ty) -> Otag (label, ty)
      | Oinherit ty -> Oinherit ty]
    else if Sys.ocaml_version >= "4.05.0" then [%e
      let (label, _, ty) = of_ in
      Otag (label, ty)]
    else [%e
      let (label, _, ty) = of_ in
      Otag (mkloc label, ty)]]

  let tag ?loc:_loc ?attrs (label : Asttypes.label Location.loc)
      (ty : Parsetree.core_type) : t =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      Ast_helper.Of.tag ?loc:_loc ?attrs label ty]
    else if Sys.ocaml_version >= "4.06.0" then [%e
      Otag (label, Option.value ~default:[] attrs, ty)]
    else if Sys.ocaml_version >= "4.05.0" then[%e
      (label, Option.value ~default:[] attrs, ty)]
    else [%e
      (label.txt, Option.value ~default:[] attrs, ty)]]

  let inherit_ ?loc:_loc ?attrs:_attrs (_ty : Parsetree.core_type) : t =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      Ast_helper.Of.mk ?loc:_loc ?attrs:_attrs (Oinherit _ty)]
    else if Sys.ocaml_version >= "4.06.0" then [%e
      Oinherit _ty]
    else [%e
      failwith
    "Metapp.Of.inherit_: inherit object field unavailable with OCaml <4.06.0"]]
end
