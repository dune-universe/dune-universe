include (Metapp_preutils :
  module type of struct include Metapp_preutils end with
  module Exp := Metapp_preutils.Exp and
  module Typ := Metapp_preutils.Typ and
  module Mod := Metapp_preutils.Mod and
  module Mty := Metapp_preutils.Mty)

(** {1 String constant destructor} *)

type string_constant = {
    s : string;
    loc : Location.t;
    delim : string option;
  }

(** More general reimplementation without magic *)

let destruct_string_constant (constant : Parsetree.constant)
    : string_constant option =
  [%meta if Sys.ocaml_version < "4.11.0" then [%e
    match constant with
    | Pconst_string (s, delim) ->
        Some { s; loc = !Ast_helper.default_loc; delim }
    | _ -> None]
  else [%e
    match constant with
    | Pconst_string (s, loc, delim) -> Some { s; loc; delim }
    | _ -> None]]

let string_of_expression (expression : Parsetree.expression) : string_constant =
  Ast_helper.with_default_loc expression.pexp_loc @@ fun () ->
  match
    match expression.pexp_desc with
    | Pexp_constant constant -> destruct_string_constant constant
    | _ -> None
  with
  | Some value -> value
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "String value expected"

let string_of_arbitrary_expression (expression : Parsetree.expression)
    : string =
  Ast_helper.with_default_loc expression.pexp_loc @@ fun () ->
  match
    match expression.pexp_desc with
    | Pexp_constant constant -> destruct_string_constant constant
    | _ -> None
  with
  | Some value -> value.s
  | _ ->
      Format.asprintf "%a" Pprintast.expression expression

(** {1 Open} *)

module Opn = struct
  [%%meta if Sys.ocaml_version >= "4.08.0" then
    [%stri type 'a t = 'a Parsetree.open_infos]
  else [%stri
    type 'a t = {
      popen_expr : 'a;
      popen_override : Asttypes.override_flag;
      popen_loc : Location.t;
      popen_attributes : Parsetree.attributes;
    }]]
end

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

type 'a comparer = 'a -> 'a -> int

let compare_pair compare_fst compare_snd (a, b) (c, d) : int =
  let o = compare_fst a c in
  if o = 0 then
    compare_snd b d
  else
    o

let rec compare_list compare_item a b =
  match a, b with
  | [], [] -> 0
  | a_hd :: a_tl, b_hd :: b_tl ->
      compare_pair compare_item (compare_list compare_item) (a_hd, a_tl)
        (b_hd, b_tl)
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1

(** {1 Module binding and declaration} *)

let _anonymous_module_unsupported =
  "Anonymous modules are not supported with OCaml <4.10.0"

[%%meta Metapp_preutils.Stri.of_list (
  if Sys.ocaml_version >= "4.10.0" then [%str
    type module_name = string option

    external module_name_of_string_option : string option -> module_name =
      "%identity"

    external string_option_of_module_name : module_name -> string option =
      "%identity"]
  else [%str
    type module_name = string

    let module_name_of_string_option (s : string option) : module_name =
      match s with
      | Some name -> name
      | None -> invalid_arg _anonymous_module_unsupported

    let string_option_of_module_name (name : module_name) : string option =
      Some name])]

module Md = struct
  let mk ?loc ?attrs (mod_name : string option Location.loc)
      (s : Parsetree.module_type) : Parsetree.module_declaration =
    Ast_helper.Md.mk ?loc ?attrs (map_loc module_name_of_string_option mod_name)
      s
end

module Mb = struct
  let mk ?loc ?attrs (mod_name : string option Location.loc)
      (s : Parsetree.module_expr) : Parsetree.module_binding =
    Ast_helper.Mb.mk ?loc ?attrs (map_loc module_name_of_string_option mod_name)
      s
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

  let destruct_open (expression : Parsetree.expression)
      : (Parsetree.module_expr Opn.t * Parsetree.expression) option =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      match expression.pexp_desc with
      | Pexp_open (open_decl, expr) ->
          Some (open_decl, expr)
      | _ ->
          None]
    else [%e
      match expression.pexp_desc with
      | Pexp_open (popen_override, module_name, expr) ->
          let open_decl : Parsetree.module_expr Opn.t = {
            popen_expr = Ast_helper.Mod.ident module_name;
            popen_override;
            popen_loc = expression.pexp_loc;
            popen_attributes = [];
          } in
          Some (open_decl, expr)
      | _ ->
          None]]

  let open_ ?loc ?attrs (open_decl : Parsetree.module_expr Opn.t)
      (expr : Parsetree.expression) : Parsetree.expression =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      Ast_helper.Exp.open_ ?loc ?attrs open_decl expr]
    else [%e
      let module_name =
        match open_decl.popen_expr.pmod_desc with
        | Pmod_ident module_name -> module_name
        | _ ->
            invalid_arg "Metapp.Exp.open_: OCaml <4.08.0 only support module identifiers in open" in
      Ast_helper.Exp.open_ ?loc ?attrs open_decl.popen_override module_name
        expr]]

  let tuple_of_payload (payload : Parsetree.payload)
      : Parsetree.expression list =
    match of_payload payload with
    | { pexp_desc = Pexp_tuple tuple } -> tuple
    | e -> [e]
end

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

  let get_derivers (attributes : Parsetree.attributes)
      : Parsetree.expression list option =
    match find "deriving" attributes with
    | None -> None
    | Some derivers -> Some (Exp.tuple_of_payload (payload derivers))

  let has_deriver (deriver_name : string) (attributes : Parsetree.attributes)
      : (Asttypes.arg_label * Parsetree.expression) list option =
    Option.bind (get_derivers attributes)
      (List.find_map (fun (e : Parsetree.expression) ->
        match e.pexp_desc with
        | Pexp_ident { txt = Lident name; _ }
          when String.equal name deriver_name ->
            Some []
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name; _ }}, args)
          when String.equal name deriver_name ->
            Some args
        | _ -> None))
end

(** {1 Type declarations} *)

module Type = struct
  let has_deriver (deriver_name : string)
      (declarations : Parsetree.type_declaration list)
      : (Asttypes.arg_label * Parsetree.expression) list option =
    declarations |> List.find_map (fun (decl : Parsetree.type_declaration) ->
      Attr.has_deriver deriver_name decl.ptype_attributes)
end

(** {1 Longident} *)

module Longident = struct
  include Longident

  let make ?prefix name =
    make_ident ?prefix name

  let rec concat (a : t) (b : t) : t =
    match b with
    | Lident b -> Ldot (a, b)
    | Ldot (m, name) -> Ldot (concat a m, name)
    | Lapply (m, x) -> Lapply (concat a m, x)

  let rec compare (a : t) (b : t) : int =
    match a, b with
    | Lident a, Lident b -> String.compare a b
    | Ldot (am, ax), Ldot (bm, bx) ->
        compare_pair compare String.compare (am, ax) (bm, bx)
    | Lapply (af, am), Lapply (bf, bm) ->
        compare_pair compare compare (af, am) (bf, bm)
    | Lident _, (Ldot _ | Lapply _)
    | Ldot _, Lapply _ -> -1
    | Lapply _, Ldot _ -> 1
    | (Ldot _ | Lapply _), Lident _ -> 1

  let equal (a : t) (b : t) : bool =
    compare a b = 0

  let rec hash (a : t) : int =
    match a with
    | Lident a -> Hashtbl.hash (1, a)
    | Ldot (a, b) -> Hashtbl.hash (hash a, b)
    | Lapply (m, x) -> Hashtbl.hash (hash m, hash x)

  let pp (fmt : Format.formatter) (ident : Longident.t) =
    [%meta if Sys.ocaml_version >= "4.08.0" then [%e
      Pprintast.longident fmt ident]
    else [%e
      Pprintast.expression fmt (Exp.ident ident)]]

  let show (ident : Longident.t) : string =
    Format.asprintf "%a" pp ident

  let of_module_expr_opt (module_expr : Parsetree.module_expr)
      : Longident.t option =
    match module_expr.pmod_desc with
    | Pmod_ident { txt; _ } -> Some txt
    | _ -> None

  let rec of_expression_opt (expression : Parsetree.expression) : t option =
    match expression.pexp_desc with
    | Pexp_ident { txt; _ } -> Some txt
    | Pexp_construct ({ txt; _ }, None) -> Some txt
    | _ ->
        Option.bind (Exp.destruct_open expression) (fun (open_decl, expr) ->
          Option.bind (of_module_expr_opt open_decl.popen_expr) (fun a ->
            Option.map (concat a) (of_expression_opt expr)))

  let of_payload_opt (payload : Parsetree.payload) : t option =
    match payload with
    | PStr [{ pstr_desc = Pstr_eval (expression, [])}] ->
        of_expression_opt expression
    | _ -> None

  let of_payload (payload : Parsetree.payload) : t =
    match of_payload_opt payload with
    | Some ident -> ident
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc "Identifier expected"
end

let mklid ?prefix name =
  mkloc (Longident.make ?prefix name)

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
      invalid_arg
    "Metapp.Of.inherit_: inherit object field unavailable with OCaml <4.06.0"]]
end

(** {1 Module expressions} *)

[%%meta Metapp_preutils.Stri.of_list (
if Sys.ocaml_version >= "4.10.0" then [%str
  type functor_parameter = Parsetree.functor_parameter =
    | Unit
    | Named of string option Location.loc * Parsetree.module_type]
else [%str
  type functor_parameter =
    | Unit
    | Named of string option Location.loc * Parsetree.module_type

  let construct_functor_parameter x t =
    match t with
    | None -> Unit
    | Some t -> Named (map_loc Option.some x, t)

  let destruct_functor_parameter p =
    match p with
    | Unit -> mkloc "", None
    | Named (name_loc, t) ->
        match name_loc.txt with
        | None -> invalid_arg _anonymous_module_unsupported
        | Some txt -> { name_loc with txt }, Some t
])]

module type FunctorS = sig
  type t

  val functor_ :
      ?loc:Location.t -> ?attrs:Parsetree.attributes -> functor_parameter ->
        t -> t

  val destruct_functor : t -> (functor_parameter * t) option
end

module type ModS = sig
  include ExtensibleS

  include FunctorS with type t := t
end

module Mod = struct
  include Metapp_preutils.Mod

  let functor_ ?loc ?attrs (parameter : functor_parameter)
      (body : Parsetree.module_expr) : Parsetree.module_expr =
    [%meta if Sys.ocaml_version >= "4.10.0" then
      [%e Ast_helper.Mod.functor_ ?loc ?attrs parameter body]
    else [%e
      let x, t = destruct_functor_parameter parameter in
      Ast_helper.Mod.functor_ ?loc ?attrs x t body]]

  let destruct_functor (modtype : Parsetree.module_expr)
      : (functor_parameter * Parsetree.module_expr) option =
    match modtype.pmod_desc with
    | [%meta if Sys.ocaml_version >= "4.10.0" then
        [%p? Pmod_functor (f, s)]
      else
        [%p? Pmod_functor (x, t, s)]] ->
        [%meta if Sys.ocaml_version >= "4.10.0" then
          [%e Some (f, s)]
        else
          [%e Some (construct_functor_parameter x t, s)]]
    | _ -> None
end

(** {1 Module types} *)

module Mty = struct
  include Metapp_preutils.Mty

  let functor_ ?loc ?attrs (parameter : functor_parameter)
      (body : Parsetree.module_type) : Parsetree.module_type =
    [%meta if Sys.ocaml_version >= "4.10.0" then
      [%e Ast_helper.Mty.functor_ ?loc ?attrs parameter body]
    else [%e
      let x, t = destruct_functor_parameter parameter in
      Ast_helper.Mty.functor_ ?loc ?attrs x t body]]

  let destruct_functor (modtype : Parsetree.module_type)
      : (functor_parameter * Parsetree.module_type) option =
    match modtype.pmty_desc with
    | [%meta if Sys.ocaml_version >= "4.10.0" then
        [%p? Pmty_functor (f, s)]
      else
        [%p? Pmty_functor (x, t, s)]] ->
        [%meta if Sys.ocaml_version >= "4.10.0" then
          [%e Some (f, s)]
        else
          [%e Some (construct_functor_parameter x t, s)]]
    | _ -> None
end

module Types = struct
  (** {1 Signature type destruction} *)

  [%%meta if Sys.ocaml_version >= "4.08.0" then [%stri
    type visibility = Types.visibility =
      | Exported
      | Hidden]
  else [%stri
    type visibility =
      | Exported
      | Hidden]]

  module Sigi = struct
    type sig_type = {
        id : Ident.t;
        decl : Types.type_declaration;
        rec_status : Types.rec_status;
        visibility : visibility;
      }

    let sig_type (sig_type : sig_type) : Types.signature_item =
      [%meta if Sys.ocaml_version >= "4.08.0" then [%expr
        Sig_type (
          sig_type.id, sig_type.decl, sig_type.rec_status, sig_type.visibility)]
      else [%expr
        Sig_type (
          sig_type.id, sig_type.decl, sig_type.rec_status)]]

    let destruct_sig_type (item : Types.signature_item) : sig_type option =
      [%meta if Sys.ocaml_version >= "4.08.0" then [%expr
        match item with
        | Sig_type (id, decl, rec_status, visibility) ->
            Some { id; decl; rec_status; visibility }
        | _ -> None]
      else [%expr
        match item with
        | Sig_type (id, decl, rec_status) ->
            Some { id; decl; rec_status; visibility = Exported }
        | _ -> None]]
  end

  (** {1 Module types in Types} *)

  [%%meta Metapp_preutils.Stri.of_list (
  if Sys.ocaml_version >= "4.10.0" then [%str
    type functor_parameter = Types.functor_parameter =
      | Unit
      | Named of Ident.t option * Types.module_type]
  else [%str
    type functor_parameter =
      | Unit
      | Named of Ident.t option * Types.module_type

    let construct_functor_parameter x t =
      match t with
      | None -> Unit
      | Some t -> Named (Option.some x, t)

    let destruct_functor_parameter p =
      match p with
      | Unit -> Ident.create_persistent "", None
      | Named (ident_opt, t) ->
          match ident_opt with
          | None -> invalid_arg _anonymous_module_unsupported
          | Some ident -> ident, Some t
  ])]

  module Mty = struct
    let functor_ (parameter : functor_parameter)
        (body : Types.module_type) : Types.module_type =
      [%meta if Sys.ocaml_version >= "4.10.0" then
        [%e Mty_functor (parameter, body)]
      else [%e
        let x, t = destruct_functor_parameter parameter in
        Mty_functor (x, t, body)]]

    let destruct_functor (modtype : Types.module_type)
        : (functor_parameter * Types.module_type) option =
      match modtype with
      | [%meta if Sys.ocaml_version >= "4.10.0" then
          [%p? Mty_functor (f, s)]
        else
          [%p? Mty_functor (x, t, s)]] ->
          [%meta if Sys.ocaml_version >= "4.10.0" then
            [%e Some (f, s)]
          else
            [%e Some (construct_functor_parameter x t, s)]]
      | _ -> None

    let destruct_alias (modtype : Types.module_type) : Path.t option =
      match modtype with
      | [%meta
         if Sys.ocaml_version >= "4.08.0" || Sys.ocaml_version < "4.04.0" then
           [%p? Mty_alias p]
         else
           [%p? Mty_alias (_, p)]] -> Some p
      | _ -> None
  end
end

(** {1 With constraint} *)

module With = struct
  let typesubst ?t:_t (decl : Parsetree.type_declaration)
      : Parsetree.with_constraint =
    [%meta if Sys.ocaml_version >= "4.06.0" then
      [%e
        let t =
          match _t with
          | None -> lid_of_str decl.ptype_name
          | Some t -> t in
        Parsetree.Pwith_typesubst (t, decl)]
    else
      [%e Parsetree.Pwith_typesubst decl]]

  let destruct_typesubst (cstr : Parsetree.with_constraint)
      : (Ast_helper.lid * Parsetree.type_declaration) option =
    [%meta if Sys.ocaml_version >= "4.06.0" then [%e
      match cstr with
      | Pwith_typesubst (t, decl) -> Some (t, decl)
      | _ -> None]
    else [%e
      match cstr with
      | Pwith_typesubst decl -> Some (lid_of_str decl.ptype_name, decl)
      | _ -> None]]

  let modsubst (x : Ast_helper.lid) (y : Ast_helper.lid)
      : Parsetree.with_constraint =
    [%meta if Sys.ocaml_version >= "4.06.0" then
      [%e Parsetree.Pwith_modsubst (x, y)]
    else
      [%e
        let x =
          match x.txt with
          | Lident txt -> { x with txt }
          | _ -> invalid_arg "Module substitution should not be qualified with OCaml <4.06" in
        Parsetree.Pwith_modsubst (x, y)]]

  let destruct_modsubst (cstr : Parsetree.with_constraint)
      : (Ast_helper.lid * Ast_helper.lid) option =
    [%meta if Sys.ocaml_version >= "4.06.0" then [%e
      match cstr with
      | Pwith_modsubst (x, y) -> Some (x, y)
      | _ -> None]
    else [%e
      match cstr with
      | Pwith_modsubst (x, y) -> Some (lid_of_str x, y)
      | _ -> None]]
end
