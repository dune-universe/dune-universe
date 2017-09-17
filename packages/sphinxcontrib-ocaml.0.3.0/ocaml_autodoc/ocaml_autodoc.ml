open General.Abbr


module J = struct
  type t = Yojson.Basic.json
  type a = string * t

  let to_string = Yojson.Basic.pretty_to_string

  let null = `Null

  let (str: string -> t) = fun x ->
    `String x

  let (li: t list -> t) = fun x ->
    `List x

  let (bo: bool -> t) = fun x ->
    `Bool x

  let (opt: t option -> t) = function
    | None -> null
    | Some x -> x

  let (obj: string -> a list -> t) = fun kind attributes ->
    `Assoc (("__class__", str kind)::attributes)
end


(*BISECT-IGNORE*) let warn s v = StdErr.print "WARNING (sphinxcontrib-ocaml): %s\n" s; v


module Name: sig
  val of_string: string -> J.a
  val of_string_loc: string Asttypes.loc -> J.a
  val of_ident: Ident.t -> J.a
end = struct
  let of_string s =
    let format =
      if Oprint.parenthesized_ident s then
        (* This is not strictly correct for ( * ) but it's prettier for everything else. *)
        Frmt.of_string "(%s)"
      else
        Frmt.of_string "%s"
    in
    ("name", J.str (Frmt.apply format s))

  let of_string_loc {Asttypes.txt; loc=_} =
    of_string txt

  let of_ident id =
    id
    |> Ident.name
    |> of_string
end


module Hidden: sig
  val default: J.a

  module OfTypedtree: sig
    open Typedtree
    val attributes: attributes -> J.a
  end
end = struct
  let of_bool hidden =
    ("hidden", J.bo hidden)

  let default = of_bool false

  module OfTypedtree = struct
    let attributes attributes =
      attributes
      |> Li.there_exists ~f:(fun ({Asttypes.txt; loc=_}, payload) ->
        txt = "autodoc.hide" && payload = Parsetree.PStr []
      )
      |> of_bool
  end
end


exception ModTypeNotFound of string


(* @todo Understand more deeply how we should add a module type's contents when we explore it.
Especialy for Mty_ident and Mty_functor *)
let enter_modtype path env =
  let signature {Types.mtd_type; _} =
    let of_module_type = function
      | Types.Mty_ident _ ->
        []
      | Types.Mty_signature signature ->
        signature
      | Types.Mty_functor (_, _, _) ->
        []
      | Types.Mty_alias (_, _) -> (*BISECT-IGNORE*) warn "enter_modtype: Mty_alias (@todo)" []
    in
    let of_module_type_option = function
      | None ->
        []
      | Some mod_type ->
        of_module_type mod_type
    in
    of_module_type_option mtd_type
  in
  (* StdErr.print "Looking-up modtype %s in:\n" (Path.name path);
  Env.fold_modtypes (fun s path _declaration () ->
    StdErr.print "  - %s: %s\n" s (Path.name path)
  ) None env (); *)
  let declaration =
    try
      Env.find_modtype path env
    with
      | Exn.NotFound -> Exn.raise (ModTypeNotFound (Path.name path))
  in
  (* StdErr.print "Entering modtype %s\n" (Path.name path); *)
  let env = Env.add_signature (signature declaration) env in
  (* StdErr.print "Entered modtype %s:\n" (Path.name path);
  Env.fold_modtypes (fun s path _declaration () ->
    StdErr.print "  - %s: %s\n" s (Path.name path)
  ) None env ();
  StdErr.print "\n"; *)
  (env, declaration)


let enter_module path env =
  (env, Env.find_module path env)


module KnownBug: sig
  val mod_type_not_found: string -> J.t
end = struct
  let mod_type_not_found name =
    J.obj "known_bug" [
      Name.of_string name;
      Hidden.default;
      ("kind", J.str "module type not found");
    ]
end


module Doc: sig
  val empty: J.a
  val merge: J.a list -> J.a

  module OfTypedtree: sig
    open Typedtree
    val attributes: attributes -> J.a
    val module_type: module_type -> J.a
    val module_type_option: module_type option -> J.a
  end

  module OfTypes: sig
    open Types
    val module_type: Env.t -> module_type -> J.a
    val module_type_option: Env.t -> module_type option -> J.a
  end
end = struct
  let of_list xs =
    ("doc", J.li xs)

  let empty = of_list []

  let merge xs =
    xs
    |> Li.flat_map ~f:(function
      | (_, `List x) ->
        x
      | _ -> (*BISECT-IGNORE*) Exn.failure "Doc.merge"
    )
    |> of_list

  let filter_attributes attributes =
    attributes
    |> Li.filter_map ~f:(fun ({Asttypes.txt; loc=_}, payload) ->
      Opt.some_if (txt = "ocaml.doc") (lazy Parsetree.(
        match payload with
          | PStr [{pstr_desc=Pstr_eval ({pexp_desc=Pexp_constant (Parsetree.Pconst_string (s, _)); _}, _); _}] ->
            J.str s
          | _ -> (*BISECT-IGNORE*) Exn.failure "Doc.of_attribute"
      ))
    )

  module OfTypes = struct
    let rec module_type_option env = function
      | None ->
        []
      | Some mod_typ ->
        module_type env mod_typ

    and modtype_declaration env {Types.mtd_type; mtd_attributes; mtd_loc=_} =
        (filter_attributes mtd_attributes) @ (module_type_option env mtd_type)

    and module_type env = function
      | Types.Mty_ident path ->
        let (env, mod_type) = enter_modtype path env in
        modtype_declaration env mod_type
      | Types.Mty_signature _ ->
        []
      | Types.Mty_functor (_, _, contents) ->
        module_type env contents
      | Types.Mty_alias (_, _) ->
        []

    let module_type_option env mod_typ =
      mod_typ
      |> module_type_option env
      |> of_list

    let module_type env mod_typ =
      mod_typ
      |> module_type env
      |> of_list

    let module_declaration _env {Types.md_type=_; md_attributes; md_loc=_} =
      md_attributes
      |> filter_attributes
      |> of_list
  end

  module OfTypedtree = struct
    let attributes attributes =
      attributes
      |> filter_attributes
      |> of_list

    let rec module_expr {Typedtree.mod_desc; mod_loc=_; mod_type=_; mod_env; mod_attributes} =
      assert (filter_attributes mod_attributes = []);
      match mod_desc with
        | Typedtree.Tmod_ident (path, _) ->
          let (env, declaration) = enter_module path mod_env in
          OfTypes.module_declaration env declaration
        | Typedtree.Tmod_structure _ ->
          empty
        | Typedtree.Tmod_functor (_, _, _, _) ->
          empty
        | Typedtree.Tmod_apply (functor_, _,_) ->
          module_expr functor_
        | Typedtree.Tmod_constraint (_, _, _, _) -> (*BISECT-IGNORE*) warn "Doc.OfTypedtree.module_expr: Tmod_constraint (not supported)" empty
        | Typedtree.Tmod_unpack (_, _) -> (*BISECT-IGNORE*) warn "Doc.OfTypedtree.module_expr: Tmod_unpack (not supported)" empty

    let rec module_type {Typedtree.mty_desc; mty_type=_; mty_env; mty_loc=_; mty_attributes} =
      assert (filter_attributes mty_attributes = []);
      match mty_desc with
        | Typedtree.Tmty_signature _ ->
          empty
        | Typedtree.Tmty_ident (path, _) ->
          let (env, mod_type) = enter_modtype path mty_env in
          OfTypes.modtype_declaration env mod_type
          |> of_list
        | Typedtree.Tmty_functor (_, _, _, contents) ->
          module_type contents
        | Typedtree.Tmty_with (mod_type, _) ->
          module_type mod_type
        | Typedtree.Tmty_typeof module_ ->
          module_expr module_
        | Typedtree.Tmty_alias (_, _) ->
          empty

    let module_type_option = Opt.value_map ~f:module_type ~def:empty
  end
end


module FloatingDoc: sig
  val of_attribute_payload: Parsetree.payload -> J.t
end = struct
  let of_attribute_payload = Parsetree.(function
    | PStr [{pstr_desc=Pstr_eval ({pexp_desc=Pexp_constant (Parsetree.Pconst_string (s, _)); _}, _); _}] ->
      J.obj "floating_documentation" [
        Hidden.default;
        ("text", J.str s);
      ]
    | _ -> (*BISECT-IGNORE*) Exn.failure "FloatingDoc.of_attribute_payload"
  )
end


let string_of_type_expr t =
  Printtyp.reset ();
  Printtyp.type_expr OCamlStandard.Format.str_formatter t;
  OCamlStandard.Format.flush_str_formatter ()

let string_of_core_type {Typedtree.ctyp_desc=_; ctyp_type; ctyp_env=_; ctyp_loc=_; ctyp_attributes=_} =
  string_of_type_expr ctyp_type

let string_of_typedtree_record labels =
  labels
  |> Li.map ~f:(fun {Typedtree.ld_id=_; ld_name={Asttypes.txt; loc=_}; ld_mutable; ld_type; ld_loc=_; ld_attributes=_} ->
    Frmt.apply "%s%s: %s" (if ld_mutable = Asttypes.Mutable then "mutable " else "") txt (string_of_core_type ld_type)
  )
  |> StrLi.join ~sep:"; "
  |> Frmt.apply "{%s}"

let string_of_types_record labels =
  labels
  |> Li.map ~f:(fun {Types.ld_id; ld_mutable; ld_type; ld_loc=_; ld_attributes=_} ->
    Frmt.apply "%s%s: %s" (if ld_mutable = Asttypes.Mutable then "mutable " else "") (Ident.name ld_id) (string_of_type_expr ld_type)
  )
  |> StrLi.join ~sep:"; "
  |> Frmt.apply "{%s}"

let string_of_typedtree_tuple elements =
  let format =
    match elements with
      | [{Typedtree.ctyp_type={Types.desc=Types.Ttuple _; _}; _}] ->
        Frmt.of_string "(%s)"
      | _ ->
        Frmt.of_string "%s"
  in
  elements
  |> Li.map ~f:string_of_core_type
  |> StrLi.join ~sep:" * "
  |> Frmt.apply format

let string_of_types_tuple elements =
  let format =
    match elements with
      | [{Types.desc=Types.Ttuple _; _}] ->
        Frmt.of_string "(%s)"
      | _ ->
        Frmt.of_string "%s"
  in
  elements
  |> Li.map ~f:string_of_type_expr
  |> StrLi.join ~sep:" * "
  |> Frmt.apply format

let rec string_of_module_expr {Typedtree.mod_desc; mod_loc=_; mod_type=_; mod_env=_; mod_attributes=_} =
  match mod_desc with
    | Typedtree.Tmod_ident (path, _) ->
      Path.name path
    | Typedtree.Tmod_structure _ ->
      "struct ... end"
    | Typedtree.Tmod_functor (_, _, _, contents) ->
      Frmt.apply "functor (...) -> %s" (string_of_module_expr contents)
    | Typedtree.Tmod_apply (functor_, parameter, _) ->
      Frmt.apply "%s(%s)" (string_of_module_expr functor_) (string_of_module_expr parameter)
    | Typedtree.Tmod_constraint (_, _, _, _) -> (*BISECT-IGNORE*) warn "string_of_module_expr: Tmod_constraint (not supported)" "..."
    | Typedtree.Tmod_unpack (_, _) -> (*BISECT-IGNORE*) warn "string_of_module_expr: Tmod_unpack (not supported)" "..."

let string_of_type_parameter (t, v) =
  let variance = match v with
    | Asttypes.Covariant ->
      "+"
    | Asttypes.Contravariant ->
      "-"
    | Asttypes.Invariant ->
      ""
  in
  Frmt.apply "%s%s" variance (string_of_core_type t)

let string_of_type_parameters = function
  | [] ->
    None
  | [type_parameter] ->
    type_parameter
    |> string_of_type_parameter
    |> Opt.some
  | type_parameters ->
    type_parameters
    |> Li.map ~f:string_of_type_parameter
    |> StrLi.join ~sep:", "
    |> Frmt.apply "(%s)"
    |> Opt.some

let rec string_of_module_type {Typedtree.mty_desc; mty_type=_; mty_env=_; mty_loc=_; mty_attributes=_} =
  match mty_desc with
    | Typedtree.Tmty_ident (path, _) ->
      Path.name path
    | Typedtree.Tmty_signature _ -> (*BISECT-IGNORE*) warn "string_of_module_type: Tmty_signature (should not happen)" "..."
    | Typedtree.Tmty_functor (_, _, _, _) -> (*BISECT-IGNORE*) warn "string_of_module_type: Tmty_functor (should not happen)" "..."
    | Typedtree.Tmty_with (base_module_type, constraints) ->
      constraints
      |> Li.map ~f:(fun (path, _, constraint_) ->
        let name = Path.name path
        and operator =
          match constraint_ with
            | (Typedtree.Twith_type _ | Typedtree.Twith_module (_, _)) ->
              "="
            | (Typedtree.Twith_typesubst _ | Typedtree.Twith_modsubst (_, _)) ->
              ":="
        in
        match constraint_ with
          | (Typedtree.Twith_type declaration | Typedtree.Twith_typesubst declaration) ->
            let {Typedtree.typ_id=_; typ_name=_; typ_params; typ_type=_; typ_cstrs=_; typ_kind=_; typ_private=_; typ_manifest; typ_loc=_; typ_attributes=_} = declaration in
            Frmt.apply
              "type %s%s %s %s"
              (typ_params |> string_of_type_parameters |> Opt.value_map ~f:(Frmt.apply "%s ") ~def:"")
              name
              operator
              (string_of_core_type (Opt.value typ_manifest))
          | (Typedtree.Twith_module (path, _) | Typedtree.Twith_modsubst (path, _)) ->
            Frmt.apply
              "module %s %s %s"
              name
              operator
              (Path.name path)
      )
      |> StrLi.join ~sep:" and "
      |> Frmt.apply "%s with %s" (string_of_module_type base_module_type)
    | Typedtree.Tmty_typeof module_ ->
      module_
      |> string_of_module_expr
      |> Frmt.apply "module type of %s"
    | Typedtree.Tmty_alias (_, _) -> (*BISECT-IGNORE*) warn "string_of_module_type: Tmty_alias (@todo)" "..."


module Payload: sig
  module OfTypedtree: sig
    open Typedtree
    val constructor_arguments: constructor_arguments -> J.a
  end

  module OfTypes: sig
    open Types
    val constructor_arguments: constructor_arguments -> J.a
  end
end = struct
  let of_string s =
    ("payload", J.str s)

  let empty = ("payload", J.null)

  module OfTypedtree = struct
    let constructor_arguments = function
      | Typedtree.Cstr_tuple [] ->
        empty
      | Typedtree.Cstr_tuple elements ->
        elements
        |> string_of_typedtree_tuple
        |> of_string
      | Typedtree.Cstr_record declarations ->
        declarations
        |> string_of_typedtree_record
        |> of_string
  end

  module OfTypes = struct
    let constructor_arguments = function
      | Types.Cstr_tuple [] ->
        empty
      | Types.Cstr_tuple elements ->
        elements
        |> string_of_types_tuple
        |> of_string
      | Types.Cstr_record declarations ->
        declarations
        |> string_of_types_record
        |> of_string
  end
end


module RecordLabels: sig
  module OfTypedtree: sig
    open Typedtree
    val constructor_arguments: constructor_arguments -> J.a
    val type_kind: type_kind -> J.a
  end

  module OfTypes: sig
    open Types
    val constructor_arguments: constructor_arguments -> J.a
    val type_kind: type_kind -> J.a
  end
end = struct
  let of_list xs =
    ("labels", J.li xs)

  let empty = of_list []

  module OfTypedtree = struct
    let label_declarations declarations =
        declarations
        |> Li.map ~f:(fun {Typedtree.ld_id=_; ld_name; ld_mutable=_; ld_type=_; ld_loc=_; ld_attributes} ->
          J.obj "record_label" [
            Name.of_string_loc ld_name;
            Doc.OfTypedtree.attributes ld_attributes;
          ]
        )

    let constructor_arguments = function
      | Typedtree.Cstr_tuple _ ->
        []
      | Typedtree.Cstr_record declarations ->
        label_declarations declarations

    let type_kind = function
      | Typedtree.Ttype_abstract ->
        empty
      | Typedtree.Ttype_variant declarations ->
        declarations
        |> Li.flat_map ~f:(fun {Typedtree.cd_id=_; cd_name=_; cd_args; cd_res=_; cd_loc=_; cd_attributes=_} ->
          constructor_arguments cd_args
        )
        |> of_list
      | Typedtree.Ttype_record declarations ->
        declarations
        |> label_declarations
        |> of_list
      | Typedtree.Ttype_open ->
        empty

    let constructor_arguments = of_list % constructor_arguments
  end

  module OfTypes = struct
    let label_declarations declarations =
        declarations
        |> Li.map ~f:(fun {Types.ld_id; ld_mutable=_; ld_type=_; ld_loc=_; ld_attributes} ->
          J.obj "record_label" [
            Name.of_ident ld_id;
            Doc.OfTypedtree.attributes ld_attributes;
          ]
        )

    let constructor_arguments = function
      | Types.Cstr_tuple _ ->
        []
      | Types.Cstr_record declarations ->
        label_declarations declarations

    let type_kind = function
      | Types.Type_abstract ->
        empty
      | Types.Type_variant declarations ->
        declarations
        |> Li.flat_map ~f:(fun {Types.cd_id=_; cd_args; cd_res=_; cd_loc=_; cd_attributes=_} ->
          constructor_arguments cd_args
        )
        |> of_list
      | Types.Type_record (declarations, _) ->
        declarations
        |> label_declarations
        |> of_list
      | Types.Type_open ->
        empty

    let constructor_arguments = of_list % constructor_arguments
  end
end


module TypeParameters: sig
  module OfTypedtree: sig
    open Typedtree
    val type_parameters: (core_type * Asttypes.variance) list -> J.a
  end

  module OfTypes: sig
    open Types
    val type_exprs_and_variances: type_expr list -> Variance.t list -> J.a
  end
end = struct
  let of_string s =
    ("parameters", J.str s)

  let empty = ("parameters", J.null)

  module OfTypedtree = struct
    let type_parameters params = 
      params
      |> string_of_type_parameters
      |> Opt.value_map ~f:of_string ~def:empty
  end

  module OfTypes = struct
    let string_of_type_parameter (t, v) =
      let variance = match Types.Variance.get_upper v with
        | (true, true)
        | (false, false) ->
          ""
        | (true, false) ->
          "+"
        | (false, true) ->
          "-"
      in
      Frmt.apply "%s%s" variance (string_of_type_expr t)

    let type_exprs_and_variances type_exprs variances =
      match Li.Two.to_pair_list type_exprs variances with
        | [] ->
          empty
        | [type_parameter] ->
          type_parameter
          |> string_of_type_parameter
          |> of_string
        | type_parameters ->
          type_parameters
          |> Li.map ~f:string_of_type_parameter
          |> StrLi.join ~sep:", "
          |> Frmt.apply "(%s)"
          |> of_string
  end
end


module TypeManifest: sig
  module OfTypedtree: sig
    open Typedtree
    val core_type_option: Asttypes.private_flag -> core_type option -> J.a
  end

  module OfTypes: sig
    open Types
    val type_expr_option: Asttypes.private_flag -> type_expr option -> J.a
  end
end = struct
  let of_string_option private_ s =
    let frmt =
      match private_ with
        | Asttypes.Private -> Frmt.of_string "private %s"
        | Asttypes.Public -> Frmt.of_string "%s"
    in
    ("manifest", s |> Opt.map ~f:(Frmt.apply frmt) |> Opt.map ~f:J.str |> J.opt)

  module OfTypedtree = struct
    let core_type_option private_ t =
      t
      |> Opt.map ~f:string_of_core_type
      |> of_string_option private_
  end

  module OfTypes = struct
    let type_expr_option private_ t =
      t
      |> Opt.map ~f:string_of_type_expr
      |> of_string_option private_
  end
end


module TypeConstructors: sig
  module OfTypedtree: sig
    open Typedtree
    val type_kind: type_kind -> J.a
  end

  module OfTypes: sig
    open Types
    val type_kind: type_kind -> J.a
  end
end = struct
  let of_list xs =
    ("constructors", J.li xs)

  let empty = of_list []

  module OfTypedtree = struct
    let type_kind = function
      | Typedtree.Ttype_abstract ->
        empty
      | Typedtree.Ttype_variant declarations ->
        declarations
        |> Li.map ~f:(fun {Typedtree.cd_id=_; cd_name; cd_args; cd_res=_; cd_loc=_; cd_attributes} ->
          J.obj "type_constructor" [
            Name.of_string_loc cd_name;
            Doc.OfTypedtree.attributes cd_attributes;
            Payload.OfTypedtree.constructor_arguments cd_args;
          ]
        )
        |> of_list
      | Typedtree.Ttype_record _ ->
        empty
      | Typedtree.Ttype_open ->
        empty
  end

  module OfTypes = struct
    let type_kind = function
      | Types.Type_abstract ->
        empty
      | Types.Type_variant declarations ->
        declarations
        |> Li.map ~f:(fun {Types.cd_id; cd_args; cd_res=_; cd_loc=_; cd_attributes} ->
          J.obj "type_constructor" [
            Name.of_ident cd_id;
            Doc.OfTypedtree.attributes cd_attributes;
            Payload.OfTypes.constructor_arguments cd_args;
          ]
        )
        |> of_list
      | Types.Type_record _ ->
        empty
      | Types.Type_open ->
        empty
  end
end


module TypeKind: sig
  module OfTypedtree: sig
    open Typedtree
    val type_kind: Asttypes.private_flag -> type_kind -> J.a
  end

  module OfTypes: sig
    open Types
    val type_kind: Asttypes.private_flag -> type_kind -> J.a
  end
end = struct
  let of_string private_ s =
    let frmt =
      match private_ with
        | Asttypes.Private -> Frmt.of_string "private %s"
        | Asttypes.Public -> Frmt.of_string "%s"
    in
    ("kind", s |> Frmt.apply frmt |> J.str)

  let empty = ("kind", J.null)

  module OfTypedtree = struct
    let type_kind private_ = function
      | Typedtree.Ttype_abstract ->
        empty
      | Typedtree.Ttype_variant declarations ->
        declarations
        |> Li.map ~f:(fun {Typedtree.cd_id=_; cd_name={Asttypes.txt; loc=_}; cd_args; cd_res=_; cd_loc=_; cd_attributes=_} ->
          let payload =
            match cd_args with
              | Typedtree.Cstr_tuple [] ->
                ""
              | Typedtree.Cstr_tuple elements ->
                elements
                |> string_of_typedtree_tuple
                |> Frmt.apply " of %s"
              | Typedtree.Cstr_record declarations ->
                declarations
                |> string_of_typedtree_record
                |> Frmt.apply " of %s"
          in
          Frmt.apply "%s%s" txt payload
        )
        |> StrLi.join ~sep:" | "
        |> of_string private_
      | Typedtree.Ttype_record declarations ->
        declarations
        |> string_of_typedtree_record
        |> of_string private_
      | Typedtree.Ttype_open ->
        of_string private_ ".."
  end

  module OfTypes = struct
    let type_kind private_ = function
      | Types.Type_abstract ->
        empty
      | Types.Type_variant declarations ->
        declarations
        |> Li.map ~f:(fun {Types.cd_id; cd_args; cd_res=_; cd_loc=_; cd_attributes=_} ->
          let payload =
            match cd_args with
              | Types.Cstr_tuple [] ->
                ""
              | Types.Cstr_tuple elements ->
                elements
                |> string_of_types_tuple
                |> Frmt.apply " of %s"
              | Types.Cstr_record declarations ->
                declarations
                |> string_of_types_record
                |> Frmt.apply " of %s"
          in
          Frmt.apply "%s%s" (Ident.name cd_id) payload
        )
        |> StrLi.join ~sep:" | "
        |> of_string private_
      | Types.Type_record (declarations, _) ->
        declarations
        |> string_of_types_record
        |> of_string private_
      | Types.Type_open ->
        of_string private_ ".."
  end
end


module Type: sig
  module OfTypedtree: sig
    open Typedtree
    val type_declaration: type_declaration -> J.t
  end

  module OfTypes: sig
    open Types
    val type_declaration: Ident.t -> type_declaration -> J.t
  end
end = struct
  module OfTypedtree = struct
    let type_declaration {Typedtree.typ_id=_; typ_name; typ_params; typ_type=_; typ_cstrs=_; typ_kind; typ_private; typ_manifest; typ_loc=_; typ_attributes} =
      J.obj "type" [
        Name.of_string_loc typ_name;
        Hidden.OfTypedtree.attributes typ_attributes;
        Doc.OfTypedtree.attributes typ_attributes;
        TypeParameters.OfTypedtree.type_parameters typ_params;
        TypeManifest.OfTypedtree.core_type_option typ_private typ_manifest;
        TypeKind.OfTypedtree.type_kind typ_private typ_kind;
        TypeConstructors.OfTypedtree.type_kind typ_kind;
        RecordLabels.OfTypedtree.type_kind typ_kind;
      ]
  end

  module OfTypes = struct
    let type_declaration id {Types.type_params; type_arity=_; type_kind; type_private; type_manifest; type_variance; type_newtype_level=_; type_loc=_; type_attributes; type_immediate=_; type_unboxed=_} =
      J.obj "type" [
        Name.of_ident id;
        Hidden.default;
        Doc.OfTypedtree.attributes type_attributes;
        TypeParameters.OfTypes.type_exprs_and_variances type_params type_variance;
        TypeManifest.OfTypes.type_expr_option type_private type_manifest;
        TypeKind.OfTypes.type_kind type_private type_kind;
        TypeConstructors.OfTypes.type_kind type_kind;
        RecordLabels.OfTypes.type_kind type_kind;
      ]
  end
end


module Exception: sig
  module OfTypedtree: sig
    open Typedtree
    val extension_constructor: extension_constructor -> J.t
  end

  module OfTypes: sig
    open Types
    val extension_constructor: Ident.t -> extension_constructor -> J.t
  end
end = struct
  module OfTypedtree = struct
    let extension_constructor {Typedtree.ext_id=_; ext_name; ext_type=_; ext_kind; ext_loc=_; ext_attributes} =
      let arguments =
        match ext_kind with
          | Typedtree.Text_decl (arguments, _) ->
            arguments
          | Typedtree.Text_rebind (_, _) -> (*BISECT-IGNORE*) Exn.failure "exception_: ext_kind=Text_rebind"
      in
      J.obj "exception" [
        Name.of_string_loc ext_name;
        Hidden.OfTypedtree.attributes ext_attributes;
        Doc.OfTypedtree.attributes ext_attributes;
        Payload.OfTypedtree.constructor_arguments arguments;
        RecordLabels.OfTypedtree.constructor_arguments arguments;
      ]
  end

  module OfTypes = struct
    let extension_constructor id {Types.ext_type_path=_; ext_type_params=_; ext_args; ext_ret_type=_; ext_private=_; ext_loc=_; ext_attributes} =
      J.obj "exception" [
        Name.of_ident id;
        Hidden.default;
        Doc.OfTypedtree.attributes ext_attributes;
        Payload.OfTypes.constructor_arguments ext_args;
        RecordLabels.OfTypes.constructor_arguments ext_args;
      ]
  end
end


module ValueType: sig
  module OfTypedtree: sig
    open Typedtree
    val core_type: core_type -> J.a
  end

  module OfTypes: sig
    open Types
    val type_expr: type_expr -> J.a
  end
end = struct
  let of_string s =
    ("type", J.str s)

  module OfTypedtree = struct
    let core_type t =
      t
      |> string_of_core_type
      |> of_string
  end

  module OfTypes = struct
    let type_expr t =
      t
      |> string_of_type_expr
      |> of_string
  end
end


module Value: sig
  module OfTypedtree: sig
    open Typedtree
    val value_description: value_description -> J.t
  end

  module OfTypes: sig
    open Types
        val value_description: Ident.t -> value_description -> J.t
  end
end = struct
  module OfTypedtree = struct
    let value_description {Typedtree.val_id=_; val_name; val_desc; val_val=_; val_prim=_; val_loc=_; val_attributes} =
      J.obj "value" [
        Name.of_string_loc val_name;
        Hidden.OfTypedtree.attributes val_attributes;
        Doc.OfTypedtree.attributes val_attributes;
        ValueType.OfTypedtree.core_type val_desc;
      ]
  end

  module OfTypes = struct
    let value_description id {Types.val_type; val_kind=_; val_loc=_; val_attributes} =
      J.obj "value" [
        Name.of_ident id;
        Hidden.default;
        Doc.OfTypedtree.attributes val_attributes;
        ValueType.OfTypes.type_expr val_type;
      ]
  end
end


module ContentsFrom: sig
  val default: J.a

  module OfTypedtree: sig
    open Typedtree
    val module_type: module_type -> J.a
    val module_type_option: module_type option -> J.a
  end

  module OfTypes: sig
    open Types
    val module_type: Env.t -> module_type -> J.a
    val module_type_option: Env.t -> module_type option -> J.a
  end
end = struct
  let of_string s =
    ("contents_from", J.str s)

  let default = ("contents_from", J.null)

  module OfTypedtree = struct
    let rec module_type ({Typedtree.mty_desc; mty_type=_; mty_env=_; mty_loc=_; mty_attributes=_} as t) =
      match mty_desc with
        | Typedtree.Tmty_signature _ ->
          default
        | Typedtree.Tmty_functor (_, _, _, contents) ->
          module_type contents
        | (Typedtree.Tmty_ident (_, _) | Typedtree.Tmty_with (_, _) | Typedtree.Tmty_typeof _) ->
          t
          |> string_of_module_type
          |> of_string
        | Typedtree.Tmty_alias (_, _) ->
          default

    let module_type_option = Opt.value_map ~f:module_type ~def:default
  end

  module OfTypes = struct
    let rec module_type env = function
      | Types.Mty_ident path ->
        path
        |> Path.name
        |> of_string
      | Types.Mty_signature _ ->
        default
      | Types.Mty_functor (_, _, contents) ->
        module_type env contents
      | Types.Mty_alias (_, _) ->
        default

    let module_type_option env = Opt.value_map ~f:(module_type env) ~def:default
  end
end


module AliasOf: sig
  val default: J.a

  module OfTypedtree: sig
    open Typedtree
    val module_type: module_type -> J.a
  end

  module OfTypes: sig
    open Types
    val module_type: Env.t -> module_type -> J.a
  end
end = struct
  let of_string s =
    ("alias_of", J.str s)

  let default = ("alias_of", J.null)

  module OfTypedtree = struct
    let module_type {Typedtree.mty_desc; mty_type=_; mty_env=_; mty_loc=_; mty_attributes=_} =
      match mty_desc with
        | Typedtree.Tmty_signature _ ->
          default
        | Typedtree.Tmty_functor (_, _, _, _) ->
          default
        | Typedtree.Tmty_ident (_, _) ->
          default
        | Typedtree.Tmty_with (_, _) ->
          default
        | Typedtree.Tmty_typeof _ ->
          default
        | Typedtree.Tmty_alias (path, _) ->
          of_string (Path.name path)
  end

  module OfTypes = struct
    let module_type _env = function
      | Types.Mty_ident _ ->
        default
      | Types.Mty_signature _ ->
        default
      | Types.Mty_functor (_, _, _) ->
        default
      | Types.Mty_alias (_, path) ->
        of_string (Path.name path)
  end
end


module rec FunctorParameters: sig
  val empty: J.a

  module OfTypedtree: sig
    open Typedtree
    val module_type: module_type -> J.a
    val module_type_option: module_type option -> J.a
  end

  module OfTypes: sig
    open Types
    val module_type: Env.t -> module_type -> J.a
    val module_type_option: Env.t -> module_type option -> J.a
  end
end = struct
  let of_list contents =
    ("functor_parameters", J.li contents)

  let empty = of_list []

  module OfTypes = struct
    let rec module_type' env = function
      | Types.Mty_ident path ->
        let (env, {Types.mtd_type; mtd_attributes=_; mtd_loc=_}) = enter_modtype path env in
        module_type_option env mtd_type
      | Types.Mty_signature _ ->
        []
      | Types.Mty_functor (parameter_name, parameter_type, contents) ->
        let parameter = J.obj "functor_parameter" [
          Name.of_ident parameter_name;
          module_type_option env parameter_type |> of_list;
          Doc.OfTypes.module_type_option env parameter_type;
          ContentsFrom.OfTypes.module_type_option env parameter_type;
          Contents.OfTypes.module_type_option env parameter_type;
        ] in
        parameter::(module_type' env contents)
      | Types.Mty_alias (_, _) ->
        []

    and module_type_option env = function
      | None ->
        []
      | Some mod_typ ->
        module_type' env mod_typ

    let modtype_declaration env {Types.mtd_type; mtd_attributes=_; mtd_loc=_} =
      module_type_option env mtd_type

    let module_type env mod_typ =
      mod_typ
      |> module_type' env
      |> of_list

    let module_type_option env mod_typ =
      mod_typ
      |> module_type_option env
      |> of_list
  end

  module OfTypedtree = struct
    let rec module_type t =
      let rec aux {Typedtree.mty_desc; mty_type; mty_env; mty_loc=_; mty_attributes=_} =
        match mty_desc with
          | Typedtree.Tmty_signature _ ->
            []
          | Typedtree.Tmty_ident (path, _) ->
            let (env, mod_type) = enter_modtype path mty_env in
            OfTypes.modtype_declaration env mod_type
          | Typedtree.Tmty_functor (_, parameter_name, parameter_type, contents) ->
            let parameter = J.obj "functor_parameter" [
              Name.of_string_loc parameter_name;
              module_type_option parameter_type;
              Doc.OfTypedtree.module_type_option parameter_type;
              ContentsFrom.OfTypedtree.module_type_option parameter_type;
              Contents.OfTypedtree.module_type_option parameter_type;
            ] in
            parameter::(aux contents)
          | Typedtree.Tmty_with (_, _) ->
            []
          | Typedtree.Tmty_typeof _ ->
            OfTypes.module_type' mty_env mty_type
          | Typedtree.Tmty_alias (_, _) ->
            []
      in
      aux t
      |> of_list

    and module_type_option = Opt.value_map ~f:module_type ~def:empty
  end
end

and Contents: sig
  module OfTypedtree: sig
    open Typedtree
    val signature: signature -> J.a
    val module_type: module_type -> J.a
    val module_type_option: module_type option -> J.a
  end

  module OfTypes: sig
    open Types
    val module_type: Env.t -> module_type -> J.a
    val module_type_option: Env.t -> module_type option -> J.a
  end
end = struct
  let of_list contents =
    ("contents", J.li contents)

  let empty = of_list []

  module OfTypes = struct
    let signature_item env = function
      | Types.Sig_value (id, description) ->
        Value.OfTypes.value_description id description
      | Types.Sig_type (id, declaration, _) ->
        Type.OfTypes.type_declaration id declaration
      | Types.Sig_typext (id, ({Types.ext_type_path=Path.Pident {Ident.name="exn"; _}; _} as ext), _) ->
        Exception.OfTypes.extension_constructor id ext
      | Types.Sig_typext (_, _, _) -> (*BISECT-IGNORE*) warn "Contents.OfTypes.signature_item: Sig_typext (not supported)" J.null
      | Types.Sig_module (id, declaration, _) ->
        Module.OfTypes.module_declaration env id declaration
      | Types.Sig_modtype (id, declaration) ->
        ModuleType.OfTypes.modtype_declaration env id declaration
      | Types.Sig_class (_, _, _) -> (*BISECT-IGNORE*) warn "Contents.OfTypes.signature_item: Sig_class (not supported)" J.null
      | Types.Sig_class_type (_, _, _) -> (*BISECT-IGNORE*) warn "Contents.OfTypes.signature_item: Sig_class_type (not supported)" J.null

    let signature_item env item =
      try
        signature_item env item
      with
        | ModTypeNotFound name ->
          StdErr.print "WARNING (sphinxcontrib-ocaml): Module type not found: %s (this is a known bug in sphinxcontrib-ocaml, we'd love some help from a compiler-libs expert)\n" name;
          KnownBug.mod_type_not_found name

    let signature env items =
      items
      |> Li.map ~f:(signature_item env)
      |> of_list

    let rec module_type env = function
      | Types.Mty_ident path ->
        let (env, {Types.mtd_type; mtd_attributes=_; mtd_loc=_}) = enter_modtype path env in
        module_type_option env mtd_type
      | Types.Mty_signature signature_ ->
        signature env signature_
      | Types.Mty_functor (_, _, contents) ->
        module_type env contents
      | Types.Mty_alias (_, _) ->
        empty

    and module_type_option env = function
      | None ->
        empty
      | Some mod_typ ->
        module_type env mod_typ

    let modtype_declaration env {Types.mtd_type; mtd_attributes=_; mtd_loc=_} =
      module_type_option env mtd_type
  end

  module OfTypedtree = struct
    let signature_item {Typedtree.sig_desc; sig_env=_; sig_loc=_} =
      match sig_desc with
        | Typedtree.Tsig_attribute ({Asttypes.txt="ocaml.text"; loc=_}, payload) ->
          [FloatingDoc.of_attribute_payload payload]
        | Typedtree.Tsig_attribute _ ->
          []
        | Typedtree.Tsig_modtype declaration -> begin
            [ModuleType.OfTypedtree.module_type_declaration declaration]
        end
        | Typedtree.Tsig_value description ->
          [Value.OfTypedtree.value_description description]
        | Typedtree.Tsig_module declaration ->
          [Module.OfTypedtree.module_declaration declaration]
        | Typedtree.Tsig_include description ->
          [Include.OfTypedtree.include_description description]
        | Typedtree.Tsig_type (_, declarations) ->
          declarations
          |> Li.map ~f:Type.OfTypedtree.type_declaration
        | Typedtree.Tsig_typext _ -> (*BISECT-IGNORE*) warn "Contents.OfTypedtree.signature: Typedtree.Tsig_typext (not supported)" []
        | Typedtree.Tsig_exception description ->
          [Exception.OfTypedtree.extension_constructor description]
        | Typedtree.Tsig_recmodule declarations ->
          declarations
          |> Li.map ~f:Module.OfTypedtree.module_declaration
        | Typedtree.Tsig_open _ ->
          []
        | Typedtree.Tsig_class _ -> (*BISECT-IGNORE*) warn "Contents.OfTypedtree.signature: Typedtree.Tsig_class (not supported)" []
        | Typedtree.Tsig_class_type _ -> (*BISECT-IGNORE*) warn "Contents.OfTypedtree.signature: Typedtree.Tsig_class_type (not supported)" []

    let signature {Typedtree.sig_items; sig_type=_; sig_final_env=_} =
      sig_items
      |> Li.flat_map ~f:signature_item
      |> of_list

    let rec module_type {Typedtree.mty_desc; mty_type; mty_env; mty_loc=_; mty_attributes=_} =
      match mty_desc with
        | Typedtree.Tmty_signature signature_ ->
          signature signature_
        | Typedtree.Tmty_ident (path, _) ->
          let (env, mod_type) = enter_modtype path mty_env in
          OfTypes.modtype_declaration env mod_type
        | Typedtree.Tmty_functor (_, _, _, contents) ->
          module_type contents
        | Typedtree.Tmty_with (_, _) ->
          OfTypes.module_type mty_env mty_type
        | Typedtree.Tmty_typeof _ ->
          OfTypes.module_type mty_env mty_type
        | Typedtree.Tmty_alias (_, _) ->
          empty

    let module_type_option = Opt.value_map ~f:module_type ~def:empty
  end
end

and ModuleType: sig
  module OfTypedtree: sig
    open Typedtree
    val module_type_declaration: module_type_declaration -> J.t
  end

  module OfTypes: sig
    open Types
    val modtype_declaration: Env.t -> Ident.t -> modtype_declaration -> J.t
  end
end = struct
  module OfTypedtree = struct
    let module_type_declaration {Typedtree.mtd_id=_; mtd_name; mtd_type; mtd_attributes; mtd_loc=_} =
      J.obj "module_type" [
        Name.of_string_loc mtd_name;
        Hidden.OfTypedtree.attributes mtd_attributes;
        Doc.(merge [OfTypedtree.attributes mtd_attributes; OfTypedtree.module_type_option mtd_type]);
        FunctorParameters.OfTypedtree.module_type_option mtd_type;
        ContentsFrom.OfTypedtree.module_type_option mtd_type;
        Contents.OfTypedtree.module_type_option mtd_type;
      ]
  end

  module OfTypes = struct
    let modtype_declaration env id {Types.mtd_type; mtd_attributes; mtd_loc=_} =
      J.obj "module_type" [
        Name.of_ident id;
        Hidden.default;
        Doc.(merge [OfTypedtree.attributes mtd_attributes; OfTypes.module_type_option env mtd_type]);
        FunctorParameters.OfTypes.module_type_option env mtd_type;
        ContentsFrom.OfTypes.module_type_option env mtd_type;
        Contents.OfTypes.module_type_option env mtd_type;
      ]
  end
end

and Include: sig
  module OfTypedtree: sig
    open Typedtree
    val include_description: include_description -> J.t
  end
end = struct
  module OfTypedtree = struct
    let include_description {Typedtree.incl_mod; incl_type=_; incl_loc=_; incl_attributes} =
      J.obj "include" [
        Hidden.OfTypedtree.attributes incl_attributes;
        Doc.(merge [OfTypedtree.attributes incl_attributes; OfTypedtree.module_type incl_mod]);
        ContentsFrom.OfTypedtree.module_type incl_mod;
        Contents.OfTypedtree.module_type incl_mod;
      ]
  end
end

and Module: sig
  module OfTypedtree: sig
    open Typedtree
    val signature: string -> signature -> J.t
    val module_declaration: module_declaration -> J.t
  end

  module OfTypes: sig
    open Types
    val module_declaration: Env.t -> Ident.t -> module_declaration -> J.t
  end
end = struct
  module OfTypedtree = struct
    let signature name signature =
      J.obj "module" [
        Name.of_string name;
        Hidden.default;
        Doc.empty;
        FunctorParameters.empty;
        ContentsFrom.default;
        AliasOf.default;
        Contents.OfTypedtree.signature signature;
      ]

    let module_declaration {Typedtree.md_id=_; md_name; md_type; md_attributes; md_loc=_} =
      J.obj "module" [
        Name.of_string_loc md_name;
        Hidden.OfTypedtree.attributes md_attributes;
        Doc.(merge [OfTypedtree.attributes md_attributes; OfTypedtree.module_type md_type]);
        FunctorParameters.OfTypedtree.module_type md_type;
        ContentsFrom.OfTypedtree.module_type md_type;
        AliasOf.OfTypedtree.module_type md_type;
        Contents.OfTypedtree.module_type md_type;
      ]
  end

  module OfTypes = struct
    let module_declaration env id {Types.md_type; md_attributes; md_loc=_} =
      J.obj "module" [
        Name.of_ident id;
        Hidden.default;
        Doc.(merge [OfTypedtree.attributes md_attributes; OfTypes.module_type env md_type]);
        FunctorParameters.OfTypes.module_type env md_type;
        ContentsFrom.OfTypes.module_type env md_type;
        AliasOf.OfTypes.module_type env md_type;
        Contents.OfTypes.module_type env md_type;
      ]
  end
end


let () =
  let (interface_file_name, include_directories) =
    match Li.of_array OCamlStandard.Sys.argv with
      | _::interface_file_name::include_directories ->
        (interface_file_name, include_directories)
      | _ -> begin
        StdErr.print "Usage: %s INTERFACE_FILE_NAME [INCLUDE_DIRECTORY, ...]\n" OCamlStandard.Sys.argv.(0);
        Exit.(exit (Failure 1)) (*BISECT-IGNORE*)
      end
  in
  Clflags.dont_write_files := true;
  Clflags.include_dirs := include_directories;
  Compmisc.init_path false;
  try
    let name =
      Str.drop_suffix ~suf:".mli" interface_file_name
    and signature =
      interface_file_name
      |> Pparse.parse_interface ~tool_name:"sphinxcontrib-ocaml" OCamlStandard.Format.err_formatter
      |> Typemod.type_interface "Foo?" (Compmisc.initial_env ())
    in
    Module.OfTypedtree.signature name signature
    |> J.to_string
    |> StdOut.print "%s\n"
  with
    exc -> begin (*BISECT-IGNORE*)
      Location.report_exception OCamlStandard.Format.err_formatter exc;
      Exit.(exit (Failure 1)) (*BISECT-IGNORE*)
    end
