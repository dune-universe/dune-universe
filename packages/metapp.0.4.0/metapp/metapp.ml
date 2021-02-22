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

let destruct_string_constant (constant : Ppxlib.constant)
    : string_constant option =
  match constant with
  | Pconst_string (s, loc, delim) ->
      Some { s; loc; delim }
  | _ -> None

let string_of_expression (expression : Ppxlib.expression) : string_constant =
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

let string_of_arbitrary_expression (expression : Ppxlib.expression)
    : string =
  Ast_helper.with_default_loc expression.pexp_loc @@ fun () ->
  match
    match expression.pexp_desc with
    | Pexp_constant constant -> destruct_string_constant constant
    | _ -> None
  with
  | Some value -> value.s
  | _ ->
      Format.asprintf "%a" Ppxlib.Pprintast.expression expression

(** {1 Open} *)

module Opn = struct
  [%%meta if Sys.ocaml_version >= "4.08.0" then
    [%stri type 'a t = 'a Ppxlib.open_infos]
  else [%stri
    type 'a t = {
      popen_expr : 'a;
      popen_override : Asttypes.override_flag;
      popen_loc : Location.t;
      popen_attributes : Ppxlib.attributes;
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

type module_name = string option

external module_name_of_string_option : string option -> module_name =
  "%identity"

external string_option_of_module_name : module_name -> string option =
  "%identity"

module Md = struct
  let mk ?loc ?attrs (mod_name : string option Location.loc)
      (s : Ppxlib.module_type) : Ppxlib.module_declaration =
    Ppxlib.Ast_helper.Md.mk ?loc ?attrs
      (map_loc module_name_of_string_option mod_name)
      s
end

module Mb = struct
  let mk ?loc ?attrs (mod_name : string option Location.loc)
      (s : Ppxlib.module_expr) : Ppxlib.module_binding =
    Ppxlib.Ast_helper.Mb.mk ?loc ?attrs
      (map_loc module_name_of_string_option mod_name)
      s
end

(** {1 Expressions} *)

module Exp = struct
  include Metapp_preutils.Exp

  let send ?loc ?attrs (expression : Ppxlib.expression)
      (str : Ppxlib.Ast_helper.str) : Ppxlib.expression =
    Ppxlib.Ast_helper.Exp.send ?loc ?attrs expression str

  let newtype ?loc ?attrs (name : Ppxlib.Ast_helper.str) (ty : Ppxlib.expression)
      : Ppxlib.expression =
    Ppxlib.Ast_helper.Exp.newtype ?loc ?attrs name ty

  let destruct_open (expression : Ppxlib.expression)
      : (Ppxlib.module_expr Opn.t * Ppxlib.expression) option =
    match expression.pexp_desc with
    | Pexp_open (open_decl, expr) ->
        Some (open_decl, expr)
    | _ ->
        None

  let open_ ?loc ?attrs (open_decl : Ppxlib.module_expr Opn.t)
      (expr : Ppxlib.expression) : Ppxlib.expression =
    Ppxlib.Ast_helper.Exp.open_ ?loc ?attrs open_decl expr

  let tuple_of_payload (payload : Ppxlib.payload)
      : Ppxlib.expression list =
    match of_payload payload with
    | { pexp_desc = Pexp_tuple tuple } -> tuple
    | e -> [e]
end

(** {1 Attribute management} *)

module Attr = struct
  let mk (name : Ppxlib.Ast_helper.str) (payload : Ppxlib.payload) =
    Ppxlib.Ast_helper.Attr.mk name payload

  let name (attribute : Ppxlib.attribute) : Ppxlib.Ast_helper.str =
    attribute.attr_name

  let payload (attribute : Ppxlib.attribute) : Ppxlib.payload =
    attribute.attr_payload

  let to_loc (attribute : Ppxlib.attribute) : Location.t =
    attribute.attr_loc

  let find (attr_name : string) (attributes : Ppxlib.attributes)
      : Ppxlib.attribute option =
    List.find_opt (fun attribute ->
      String.equal (name attribute).txt attr_name) attributes

  let chop (attr_name : string) (attributes : Ppxlib.attributes)
      : (Ppxlib.attribute * Ppxlib.attributes) option =
    extract_first (fun attribute ->
      if String.equal (name attribute).txt attr_name then
        Some attribute
      else
        None) attributes

  let get_derivers (attributes : Ppxlib.attributes)
      : Ppxlib.expression list option =
    match find "deriving" attributes with
    | None -> None
    | Some derivers -> Some (Exp.tuple_of_payload (payload derivers))

  let has_deriver (deriver_name : string) (attributes : Ppxlib.attributes)
      : (Ppxlib.Asttypes.arg_label * Ppxlib.expression) list option =
    Option.bind (get_derivers attributes)
      (List.find_map (fun (e : Ppxlib.expression) ->
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
      (declarations : Ppxlib.type_declaration list)
      : (Ppxlib.Asttypes.arg_label * Ppxlib.expression) list option =
    declarations |> List.find_map (fun (decl : Ppxlib.type_declaration) ->
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
    Ppxlib.Pprintast.expression fmt (Ppxlib.Ast_helper.Exp.ident (mkloc ident))

  let show (ident : Longident.t) : string =
    Format.asprintf "%a" pp ident

  let of_module_expr_opt (module_expr : Ppxlib.module_expr)
      : Longident.t option =
    match module_expr.pmod_desc with
    | Pmod_ident { txt; _ } -> Some txt
    | _ -> None

  let rec of_expression_opt (expression : Ppxlib.expression) : t option =
    match expression.pexp_desc with
    | Pexp_ident { txt; _ } -> Some txt
    | Pexp_construct ({ txt; _ }, None) -> Some txt
    | _ ->
        Option.bind (Exp.destruct_open expression) (fun (open_decl, expr) ->
          Option.bind (of_module_expr_opt open_decl.popen_expr) (fun a ->
            Option.map (concat a) (of_expression_opt expr)))

  let of_payload_opt (payload : Ppxlib.payload) : t option =
    match payload with
    | PStr [{ pstr_desc = Pstr_eval (expression, [])}] ->
        of_expression_opt expression
    | _ -> None

  let of_payload (payload : Ppxlib.payload) : t =
    match of_payload_opt payload with
    | Some ident -> ident
    | _ ->
        Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc "Identifier expected"
end

let mklid ?prefix name =
  mkloc (Longident.make ?prefix name)

(** {1 Mapper for [[@if bool]] notation} *)

class filter =
  let check_attr (attributes : Ppxlib.attributes) =
    match Attr.find "if" attributes with
    | None -> true
    | Some attr -> bool_of_payload (Attr.payload attr) in
  let rec check_pat (p : Ppxlib.pattern) =
    begin match p.ppat_desc with
    | Ppat_constraint (p, _) -> check_pat p
    | _ -> false
    end ||
    check_attr p.ppat_attributes in
  let check_value_binding (binding : Ppxlib.value_binding) =
    check_attr binding.pvb_attributes in
  let check_value_description (description : Ppxlib.value_description) =
    check_attr description.pval_attributes in
  let check_case (case : Ppxlib.case) =
    check_pat case.pc_lhs in
  let check_expr (e : Ppxlib.expression) =
    check_attr e.pexp_attributes in
  let check_pat_snd (type a) (arg : a * Ppxlib.pattern) =
    check_pat (snd arg) in
  let check_expr_snd (type a) (arg : a * Ppxlib.expression) =
    check_expr (snd arg) in
  let check_type_declaration (declaration : Ppxlib.type_declaration) =
    check_attr declaration.ptype_attributes in
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! pattern (p : Ppxlib.pattern) : Ppxlib.pattern =
      let p = super#pattern p in
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
      | _ -> p

    method! expression (e : Ppxlib.expression) : Ppxlib.expression =
      Ppxlib.Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
      let e = super#expression e in
      match e.pexp_desc with
      | Pexp_let (rec_flag, bindings, body) ->
          begin match List.filter check_value_binding bindings with
          | [] -> body
          | bindings ->
              { e with pexp_desc = Pexp_let (rec_flag, bindings, body) }
          end
      | Pexp_fun (_label, _default, pat, body) when not (check_pat pat) ->
          body
      | Pexp_function cases ->
          { e with pexp_desc = Pexp_function (List.filter check_case cases)}
      | Pexp_apply (f, args) ->
          let items =
            List.filter check_expr_snd ((Ppxlib.Asttypes.Nolabel, f) :: args) in
          begin match
            extract_first
              (function (Ppxlib.Asttypes.Nolabel, f) -> Some f | _ -> None)
              items
          with
          | None ->
              Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
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
            Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
              "Cannot construct an empty record";
          { e with pexp_desc = Pexp_record (fields, base)}
      | Pexp_array args ->
          { e with pexp_desc = Pexp_array (List.filter check_expr args)}
      | Pexp_sequence (a, b) when not (check_expr a) -> b
      | Pexp_sequence (a, b) when not (check_expr b) -> a
      | _ -> e

    method! structure_item (item : Ppxlib.structure_item) :
        Ppxlib.structure_item =
      let item = super#structure_item item in
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
            Pstr_type
              (rec_flag, List.filter check_type_declaration declarations)}
      | _ -> item

    method! signature_item (item : Ppxlib.signature_item) :
        Ppxlib.signature_item =
      let item = super#signature_item item in
      match item.psig_desc with
      | Psig_value description when not (check_value_description description) ->
          Sigi.of_list []
      | Psig_type (rec_flag, declarations) ->
          { item with psig_desc =
            Psig_type
              (rec_flag, List.filter check_type_declaration declarations)}
      | _ -> item
  end

(** {1 Type construction} *)

module Typ = struct
  include Metapp_preutils.Typ

  let poly (names : Ppxlib.Ast_helper.str list) (ty : Ppxlib.core_type)
      : Ppxlib.core_type =
    Ppxlib.Ast_helper.Typ.poly names ty

  let poly_name name =
    (name : Ppxlib.Ast_helper.str).txt
end

(** {1 Row fields} *)

module Rf = struct
  type desc = Ppxlib.row_field_desc =
    | Rtag of Asttypes.label Location.loc * bool * Ppxlib.core_type list
    | Rinherit of Ppxlib.core_type

  let to_loc (rf : Ppxlib.row_field) : Location.t =
    rf.prf_loc

  let to_attributes (rf : Ppxlib.row_field) : Ppxlib.attributes =
    rf.prf_attributes

  let destruct (rf : Ppxlib.row_field) : desc =
    rf.prf_desc

  let tag ?loc:_loc ?attrs (label : Ppxlib.Asttypes.label Location.loc)
      (has_constant : bool) (args : Ppxlib.core_type list)
      : Ppxlib.row_field =
    Ppxlib.Ast_helper.Rf.tag ?loc:_loc ?attrs label has_constant args

  let inherit_ ?loc:_loc ?attrs:_attrs (core_type : Ppxlib.core_type)
      : Ppxlib.row_field =
    Ppxlib.Ast_helper.Rf.mk ?loc:_loc ?attrs:_attrs (Rinherit core_type)
end

(** {1 Object fields} *)

module Of = struct
  type t = Ppxlib.object_field

  type desc = Ppxlib.object_field_desc =
    | Otag of Asttypes.label Location.loc * Ppxlib.core_type
    | Oinherit of Ppxlib.core_type

  let to_loc (of_ : t) : Location.t =
    of_.pof_loc

  let to_attributes (of_ : t) : Ppxlib.attributes =
    of_.pof_attributes

  let destruct (of_ : t) : desc =
    of_.pof_desc

  let tag ?loc:_loc ?attrs (label : Asttypes.label Location.loc)
      (ty : Ppxlib.core_type) : t =
    Ppxlib.Ast_helper.Of.tag ?loc:_loc ?attrs label ty

  let inherit_ ?loc:_loc ?attrs:_attrs (_ty : Ppxlib.core_type) : t =
    Ppxlib.Ast_helper.Of.mk ?loc:_loc ?attrs:_attrs (Oinherit _ty)
end

(** {1 Module expressions} *)

type functor_parameter = Ppxlib.functor_parameter =
  | Unit
  | Named of string option Location.loc * Ppxlib.module_type

module type FunctorS = sig
  type t

  val functor_ :
      ?loc:Location.t -> ?attrs:Ppxlib.attributes -> functor_parameter ->
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
      (body : Ppxlib.module_expr) : Ppxlib.module_expr =
    Ppxlib.Ast_helper.Mod.functor_ ?loc ?attrs parameter body

  let destruct_functor (modtype : Ppxlib.module_expr)
      : (functor_parameter * Ppxlib.module_expr) option =
    match modtype.pmod_desc with
    | Pmod_functor (f, s) ->
        Some (f, s)
    | _ -> None
end

(** {1 Module types} *)

module Mty = struct
  include Metapp_preutils.Mty

  let functor_ ?loc ?attrs (parameter : functor_parameter)
      (body : Ppxlib.module_type) : Ppxlib.module_type =
    Ppxlib.Ast_helper.Mty.functor_ ?loc ?attrs parameter body

  let destruct_functor (modtype : Ppxlib.module_type)
      : (functor_parameter * Ppxlib.module_type) option =
    match modtype.pmty_desc with
    | Pmty_functor (f, s) ->
        Some (f, s)
    | _ -> None
end

let _anonymous_module_unsupported =
  "Anonymous modules are not supported with OCaml <4.10.0"

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
  let typesubst ?t (decl : Ppxlib.type_declaration)
      : Ppxlib.with_constraint =
    let t =
      match t with
      | None -> lid_of_str decl.ptype_name
      | Some t -> t in
    Ppxlib.Pwith_typesubst (t, decl)

  let destruct_typesubst (cstr : Ppxlib.with_constraint)
      : (Ppxlib.Ast_helper.lid * Ppxlib.type_declaration) option =
    match cstr with
    | Pwith_typesubst (t, decl) -> Some (t, decl)
    | _ -> None

  let modsubst (x : Ppxlib.Ast_helper.lid) (y : Ppxlib.Ast_helper.lid)
      : Ppxlib.with_constraint =
    Ppxlib.Pwith_modsubst (x, y)

  let destruct_modsubst (cstr : Ppxlib.with_constraint)
      : (Ppxlib.Ast_helper.lid * Ppxlib.Ast_helper.lid) option =
    match cstr with
    | Pwith_modsubst (x, y) -> Some (x, y)
    | _ -> None
end
