module H = Stubgen_common.Clang_helper

module HashtblExt (Key : Hashtbl.HashedType) = struct
  include Hashtbl.Make (Key)

  let find_default ~default tbl key =
    match find_opt tbl key with
    | None ->
        let value = default () in
        add tbl key value;
        value
    | Some value -> value
end

module StringHashtbl = HashtblExt (struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end)

module TypeHashtbl = HashtblExt (Clang.Type)

type enum_constant = {
    constant_name : string;
    converted_name : string;
  }

type enum_decl = {
    name : string;
    ocaml_type_name : string;
    c_type_name : string;
    c_type_conversion_function : string;
    constants : enum_constant list;
  }

type ocaml_type_conversion =
  | NoConversion
  | Expr
  | Decl
  | OMPTraitInfo
  | TypeLoc
  | DeclarationName

type type_info = {
    ocaml_type : Ppxlib.core_type;
    type_conversion : ocaml_type_conversion;
    interface_type : Clang.Type.t;
    multiple : bool;
    access : Clang.Expr.t -> Clang.Expr.t;
    default : Clang.Expr.t;
  }

module StringMap = Map.Make (String)

let clang__bindings = "Clang__bindings"

let rec get_type_info (qual_type : Clang.Lazy.Type.t)
    (enums : enum_decl StringMap.t) : type_info =
  let get_cursor_tu =
    lazy (H.call (H.decl_of_string "getCursorTU")
      [H.decl_of_string "cursor"]) in
  match Lazy.force qual_type.desc with
  | Pointer { desc = lazy (Record { name = IdentifierName "Expr"})} ->
      { ocaml_type = [%type: 'expr];
        type_conversion = Expr;
        interface_type = H.cxcursor;
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "MakeCXCursor")
            [e; Lazy.force get_cursor_tu]);
        default =
          H.call (H.decl_of_string "MakeCXCursorInvalid")
            [H.decl_of_string "CXCursor_InvalidCode";
              Lazy.force get_cursor_tu]}
  | Record { name = IdentifierName "VersionTuple" } ->
      { ocaml_type = [%type: Clang__bindings.clang_ext_versiontuple];
        type_conversion = NoConversion;
        interface_type =
          H.elaborated Struct
            (H.record (Clang.Ast.identifier_name "clang_ext_VersionTuple"));
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "makeVersionTuple") [e]);
        default = H.decl_of_string "zeroVersionTuple"}
  | Record { name = IdentifierName "StringRef" }
  | Pointer { desc = lazy (BuiltinType Char_S) } ->
      { ocaml_type = [%type: string];
        type_conversion = NoConversion;
        interface_type =
          H.typedef (Clang.Ast.identifier_name "CXString");
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "cxstring_createDup")
            [e]);
        default =
          H.call (H.decl_of_string "cxstring_createRef")
            [H.string ""] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "IdentifierInfo" }) } ->
      { ocaml_type = [%type: string];
        type_conversion = NoConversion;
        interface_type =
          H.typedef (Clang.Ast.identifier_name "CXString");
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "cxstring_createDup")
            [H.call (H.arrow e (H.field_name
              (Clang.Ast.identifier_name "getName"))) []]);
        default =
          H.call (H.decl_of_string "cxstring_createRef")
            [H.string ""] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "FunctionDecl" }) } ->
      { ocaml_type = [%type: 'declaration_name];
        type_conversion = DeclarationName;
        interface_type =
          H.elaborated Struct
            (H.record
              (Clang.Ast.identifier_name "clang_ext_DeclarationName"));
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "MakeDeclarationName")
            [H.call (H.arrow e (H.field_name
              (Clang.Ast.identifier_name "getDeclName"))) [];
             Lazy.force get_cursor_tu]);
        default =
          H.call (H.decl_of_string "MakeDeclarationNameInvalid")
            [Lazy.force get_cursor_tu] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "TypeSourceInfo" }) } ->
      { ocaml_type = [%type: 'qual_type];
        type_conversion = TypeLoc;
        interface_type =
          H.elaborated Struct
            (H.record (Clang.Ast.identifier_name "clang_ext_TypeLoc"));
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "MakeTypeLoc")
            [H.call (H.arrow e (H.field_name
              (Clang.Ast.identifier_name "getTypeLoc"))) [];
             Lazy.force get_cursor_tu]);
        default =
          H.call (H.decl_of_string "MakeTypeLocInvalid")
            [Lazy.force get_cursor_tu] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "OMPTraitInfo" }) } ->
      { ocaml_type = [%type: 'omp_trait_info];
        type_conversion = OMPTraitInfo;
        interface_type =
          H.elaborated Struct
            (H.record (Clang.Ast.identifier_name "clang_ext_OMPTraitInfo"));
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "MakeOMPTraitInfo")
            [e; Lazy.force get_cursor_tu]);
        default =
          H.call (H.decl_of_string "MakeOMPTraitInfoInvalid")
            [Lazy.force get_cursor_tu] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "MSGuidDecl" }) } ->
      { ocaml_type = [%type: 'decl];
        type_conversion = Decl;
        interface_type = H.cxcursor;
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "MakeCXCursor")
            [e; Lazy.force get_cursor_tu]);
        default =
          H.call (H.decl_of_string "MakeCXCursorInvalid")
            [H.decl_of_string "CXCursor_InvalidCode";
              Lazy.force get_cursor_tu]}
  | Pointer { desc = lazy (
        Record { name = IdentifierName "VarDecl" }) } ->
      { ocaml_type = [%type: 'decl];
        type_conversion = Decl;
        interface_type = H.cxcursor;
        multiple = false;
        access = (fun e ->
          H.call (H.decl_of_string "MakeCXCursor")
            [e; Lazy.force get_cursor_tu]);
        default =
          H.call (H.decl_of_string "MakeCXCursorInvalid")
            [H.decl_of_string "CXCursor_InvalidCode";
              Lazy.force get_cursor_tu]}
  | Record { name = IdentifierName "ParamIdx" } ->
      { ocaml_type =  [%type: int];
        type_conversion = NoConversion;
        interface_type = H.unsigned_int;
        multiple = false;
        access =
          (fun e -> H.call (H.decl_of_string "unsigned_int_of_ParamIdx") [e]);
        default = H.const_int 0; }
  | BuiltinType Int ->
      { ocaml_type =  [%type: int];
        type_conversion = NoConversion;
        interface_type = H.int;
        multiple = false;
        access = Fun.id;
        default = H.const_int 0; }
  | BuiltinType UInt ->
      { ocaml_type =  [%type: int];
        type_conversion = NoConversion;
        interface_type = H.unsigned_int;
        multiple = false;
        access = Fun.id;
        default = H.const_int 0; }
  | BuiltinType Bool ->
      { ocaml_type =  [%type: bool];
        type_conversion = NoConversion;
        interface_type = H.bool;
        multiple = false;
        access = Fun.id;
        default = H.const_bool false; }
  | Pointer qual_type ->
      let type_info = get_type_info qual_type enums in
      { type_info with
        ocaml_type = [%type: [%t type_info.ocaml_type] list];
        multiple = true;
      }
  | Enum { name = IdentifierName name; _ } ->
      begin match StringMap.find_opt name enums with
      | Some enum_decl ->
          { ocaml_type =
            Ppxlib.Ast_helper.Typ.constr
              (Metapp.mklid ~prefix:(Lident clang__bindings)
                 enum_decl.ocaml_type_name) [];
            type_conversion = NoConversion;
            interface_type =
            H.elaborated Enum
              (H.enum (Clang.Ast.identifier_name enum_decl.c_type_name));
            multiple = false;
            access = (fun e ->
              H.call (H.decl_of_string enum_decl.c_type_conversion_function) [e]);
            default =
            H.decl_of_string (List.hd enum_decl.constants).converted_name;
          }
      | None -> assert false
      end
  | _ ->
      Format.eprintf "Unsupported type %a@."
        (Refl.pp [%refl:Clang.Lazy.Ast.qual_type] []) qual_type;
      { ocaml_type =  [%type: bool];
        type_conversion = NoConversion;
        interface_type = H.bool;
        multiple = false;
        access = Fun.id;
        default = H.const_bool false; }

type argument_decl = {
    name : string;
    type_info : type_info;
  }

let make_argument_decl name qual_type enums =
  { name; type_info = get_type_info qual_type enums }

type argument_attribute = {
    name : string;
    reduced_name : string;
    getter : string;
    getter_result_type : Clang.Lazy.Type.t;
  }

type argument_desc = {
    type_info : type_info;
    mutable attributes : argument_attribute list;
    getter_name_ref : string ref;
  }

type ocaml_argument = {
    name : string;
    ty : Ppxlib.core_type;
    type_conversion : ocaml_type_conversion;
    multiple : bool;
    getter_name_ref : string ref;
  }

type ocaml_attribute = {
    name : string;
    arguments : ocaml_argument list;
  }

type context = {
    argument_table : argument_desc TypeHashtbl.t StringHashtbl.t;
    mutable constructors : Ppxlib.constructor_declaration list;
    mutable decls : Clang.Decl.t list;
    mutable protos : Clang.Decl.t list;
    mutable attributes : ocaml_attribute list;
  }

let register_argument context name reduced_name
    (argument : argument_decl) (getter, getter_result_type) =
  let type_table =
    StringHashtbl.find_default context.argument_table argument.name
      ~default:(fun () -> TypeHashtbl.create 17) in
  let desc =
    TypeHashtbl.find_default type_table argument.type_info.interface_type
      ~default:(fun () ->
        { type_info = argument.type_info; attributes = [];
          getter_name_ref = ref "" }) in
  desc.attributes <-
    { name; reduced_name; getter; getter_result_type } :: desc.attributes;
  desc.getter_name_ref

type constant = {
    name : string;
    value : int;
  }

let get_constants
    (constants : Clang.Lazy.Ast.enum_constant list) : constant list =
   constants |>
   List.map (fun (enum_constant : Clang.Lazy.Ast.enum_constant) ->
     { name = (Lazy.force enum_constant.desc).constant_name;
       value = Clang.Lazy.Enum_constant.get_value enum_constant })

let get_public_fields (fields : Clang.Lazy.Decl.annotated_field list) :
    Clang.Lazy.Ast.decl list =
  let filter (annotated_field : Clang.Lazy.Decl.annotated_field) =
    match annotated_field.specifier with
    | CXXPublic -> Some annotated_field.decl
    | _ -> None in
  List.filter_map filter fields

type spelling = {
    has_get_semantic_spelling : bool;
    constants : constant list;
  }

let find_spelling (fields : Clang.Lazy.Ast.decl list) : spelling option =
  let is_get_semantic_spelling (field : Clang.Lazy.Ast.decl) =
    match Lazy.force field.desc with
    | CXXMethod { function_decl = {
          name = IdentifierName "getSemanticSpelling"; _ }; _ } ->
        true
    | _ -> false in
  let get_spelling (field : Clang.Lazy.Ast.decl) =
    match Lazy.force field.desc with
    | EnumDecl { name = "Spelling"; constants; _ } ->
        Some (get_constants constants)
    | _ -> None in
  List.find_map get_spelling fields |> Option.map (fun constants ->
    { has_get_semantic_spelling = List.exists is_get_semantic_spelling fields;
      constants })

let enumerate_methods (fields : Clang.Lazy.Ast.decl list) :
    (string * Clang.Lazy.Type.t) StringHashtbl.t =
  let table = StringHashtbl.create 17 in
  let add_field (field : Clang.Lazy.Ast.decl) =
    match Lazy.force field.desc with
    | CXXMethod {
      function_decl = { name = IdentifierName name; function_type }; _ } ->
        let normalized_name =
          String.lowercase_ascii name |>
          Stubgen_common.option_apply
            (Stubgen_common.String_utils.remove_prefix ~prefix:"get") |>
          Stubgen_common.option_apply
            (Stubgen_common.String_utils.remove_suffix ~suffix:"loc") in
        StringHashtbl.add table normalized_name (name, function_type.result)
    | _ -> () in
  fields |> List.iter add_field;
  table

let get_reduced_attribute_name attribute =
  Option.get
    (Stubgen_common.String_utils.remove_suffix attribute ~suffix:"Attr")

let remove_trailing_underscore argument =
  Stubgen_common.option_apply
    (Stubgen_common.String_utils.remove_suffix ~suffix:"_") argument

let get_type_spelling_name name =
  Printf.sprintf "clang_ext_%s_spelling" name

let cursor = "cursor"

let attr = "attr"

let qual_attr = "qual_attr"

let parameter_cursor = H.parameter H.cxcursor cursor

let get_cursor_attr =
  Clang.Ast.node (Clang.Ast.Decl [
    H.var H.auto attr ~init:(H.call
      (H.decl_of_string "GetCursorAttr")
      [H.decl_of_string cursor])])

let cast attr qual_attr class_name body =
  Clang.Ast.node (Clang.Ast.if_
        ~condition_variable:(Clang.Ast.node (Clang.Ast.var
          qual_attr H.auto
          ~var_init:(H.call
             (Clang.Ast.node (Clang.Ast.DeclRef
               (Clang.Ast.identifier_name "dyn_cast_or_null"
                 ~nested_name_specifier:[Clang.Ast.NamespaceName "llvm"]
                 ~template_arguments:[
                   Type (Clang.Type.make (Clang.Ast.Record
                     (Clang.Ast.identifier_name class_name
                     ~nested_name_specifier:[
                       Clang.Ast.NamespaceName "clang"])))])))
             [H.decl_of_string attr])))
        (Clang.Ast.node (Clang.Ast.DeclRef
          (Clang.Ast.identifier_name qual_attr))) body)

let namespace_clang = Clang.Ast.NamespaceName "clang"

let add_fun_decl context fun_decl =
  context.decls <- H.function_decl fun_decl :: context.decls;
  context.protos <-
    H.function_decl { fun_decl with body = None } :: context.protos

let unkeyword name =
  match name with
  | "type"
  | "module" -> name ^ "_"
  | _ -> name

let restrict_decl_version (major, minor) body =
  H.directive (Ifndef (
    Printf.sprintf "LLVM_VERSION_BEFORE_%d_%d_0" major minor)) ::
  body @ [H.directive Endif]

let restrict_statement_version (major, minor) body =
  H.decl [H.directive (Ifndef (
    Printf.sprintf "LLVM_VERSION_BEFORE_%d_%d_0" major minor))] ::
  body @ [H.decl [H.directive Endif]]

let find_version_constraint versions attribute =
  match StringMap.find_opt attribute versions with
  | None
  | Some (3, 4) -> None
  | result -> result

let generate_attribute context versions name reduced_name public_methods
    (spelling : spelling option) (arguments : argument_decl list) =
  let arguments =
    arguments |> List.map @@ fun (argument : argument_decl) ->
      let arg_name = remove_trailing_underscore argument.name in
      let argument = { argument with name = arg_name } in
      match
        StringHashtbl.find_opt public_methods (String.lowercase_ascii arg_name)
      with
      | None ->
          Format.fprintf Format.err_formatter
            "No getter for %s in %s@." arg_name name;
          assert false
      | Some getter_info ->
          let getter_name_ref =
            register_argument context name reduced_name argument getter_info in
          argument, getter_info, getter_name_ref in
  let make_ocaml_argument
      ((argument : argument_decl), _getter_info, getter_name_ref) =
    if Stubgen_common.String_utils.has_suffix ~suffix:"_Size" argument.name
        || Stubgen_common.String_utils.has_suffix ~suffix:"Length" argument.name then
      None
    else
      let name =
        argument.name |>
        Stubgen_common.option_apply
          (Stubgen_common.String_utils.remove_suffix ~suffix:"Param") |>
        Stubgen_common.uncamelcase |>
        unkeyword in
      Some { name;
        ty = argument.type_info.ocaml_type;
        type_conversion = argument.type_info.type_conversion;
        multiple = argument.type_info.multiple;
        getter_name_ref } in
  let arguments =
    List.filter_map make_ocaml_argument arguments in
  let spelling =
    spelling |> Option.map (fun spelling ->
      let spelling_getter_name =
        Printf.sprintf "clang_ext_%s_getSpelling" reduced_name in
      (spelling, get_type_spelling_name reduced_name, spelling_getter_name)) in
  let arguments =
    match spelling with
    | None -> arguments
    | Some (_, type_spelling_name, spelling_getter_name) ->
      { name = "spelling";
        ty = Ppxlib.Ast_helper.Typ.constr
          (Metapp.mklid ~prefix:(Lident clang__bindings)
             (String.lowercase_ascii type_spelling_name)) [];
        type_conversion = NoConversion;
        multiple = false;
        getter_name_ref = ref spelling_getter_name } :: arguments in
  let args : Ppxlib.constructor_arguments =
    match arguments with
    | [argument] ->
        Pcstr_tuple [argument.ty]
    | _ ->
        Pcstr_record (arguments |> List.map (
        fun (argument : ocaml_argument) ->
          Ppxlib.Ast_helper.Type.field (Metapp.mkloc argument.name) argument.ty)) in
  let constructor =
    Ppxlib.Ast_helper.Type.constructor (Metapp.mkloc reduced_name) ~args in
  context.constructors <- constructor :: context.constructors;
  spelling |> Option.iter (
  fun (spelling, type_spelling_name, spelling_getter_name) ->
    let constant_names = spelling.constants |> List.map
      (fun (constant : constant) ->
      constant, Printf.sprintf "clang_ext_%s_%s" reduced_name constant.name) in
    let last_constant = snd (List.hd (List.rev constant_names)) in
    let enum_constants = constant_names |> List.map (fun (_, constant) ->
      H.enum_constant constant) in
    let spelling_enum = H.enum_decl type_spelling_name enum_constants in
    let cases =
      constant_names |> List.concat_map (fun (orig, prefixed) ->
        if orig.name = "SpellingNotCalculated" then
          []
        else
          begin
            let case_expr =
              if spelling.has_get_semantic_spelling then
                H.decl_of_string orig.name
                  ~nested_name_specifier:[
                namespace_clang;
                TypeSpec (H.record (Clang.Ast.identifier_name name));
                TypeSpec (H.record (Clang.Ast.identifier_name "Spelling"))]
              else
                H.const_int orig.value in
            let case =
              H.case case_expr
                (H.return (Some (H.decl_of_string prefixed))) in
            Option.fold (find_version_constraint versions prefixed)
              ~none:Fun.id ~some:restrict_statement_version [case]
          end) in
    let field_name =
      if spelling.has_get_semantic_spelling then
        "getSemanticSpelling"
      else
        "getSpellingListIndex" in
    let switch =
      cast attr qual_attr name
        (H.switch (H.call (H.arrow (H.decl_of_string qual_attr)
          (H.field_name (Clang.Ast.identifier_name field_name))) [])
          (H.compound cases)) in
    let return_default =
      H.return (Some (H.decl_of_string last_constant)) in
    let version_constraint =
      max (Some (3, 5)) (max
        (find_version_constraint versions reduced_name)
        (find_version_constraint versions type_spelling_name)) in
    let body =
      Option.fold version_constraint
        ~none:Fun.id ~some:restrict_statement_version
        [get_cursor_attr; switch] in
    let list = body @ [return_default] in
    let result =
      H.elaborated Enum (H.enum (Clang.Ast.identifier_name type_spelling_name)) in
    let spelling_getter =
      Clang.Ast.function_decl (Clang.Ast.function_type
        ~parameters:(Clang.Ast.parameters [parameter_cursor]) result)
        (IdentifierName spelling_getter_name) ~body:(H.compound list) in
    context.protos <- spelling_enum :: context.protos;
    add_fun_decl context spelling_getter);
  context.attributes <- { name = reduced_name; arguments } :: context.attributes

let is_parameter_base_class name =
  match name with
  | "Attr" | "TypeAttr" | "StmtAttr" | "InheritableAttr"
  | "InheritableParamAttr" | "ParameterABIAttr" -> true
  | _ -> false

let do_decl context versions (decl : Clang.Lazy.Decl.t) =
  match Lazy.force decl.desc with
  | RecordDecl {
        keyword = Class; name; fields = fields; bases = [
          { qual_type = { desc = lazy (Record {
              name = IdentifierName base_class; _ }); _}; _}]; _} when
    is_parameter_base_class base_class && not (is_parameter_base_class name) ->
      let extract_enum
          (field : Clang.Lazy.Decl.t) : (string * enum_decl) option =
        match Lazy.force field.desc with
        | EnumDecl { name = enum_name; constants; _ } ->
            let c_type_name = Printf.sprintf "clang_ext_%s_%s" name enum_name in
            let constants =
              constants |> List.map (fun ({ desc = lazy { constant_name; _ }}
                  : Clang.Lazy.Ast.enum_constant) ->
                  let converted_name =
                    Printf.sprintf "%s_%s" c_type_name constant_name in
                  { constant_name; converted_name }) in
            let enum_decl = {
              name = enum_name;
              ocaml_type_name = String.lowercase_ascii c_type_name;
              c_type_name;
              c_type_conversion_function =
                Printf.sprintf "convert_%s_%s" name enum_name;
              constants;
            } in
            Some (enum_name, enum_decl)
        | _ -> None in
      let enums, fields =
        match fields with
        | { desc = lazy (AccessSpecifier CXXPublic) } :: tail ->
            let enums, tail =
              Clang.extract_prefix_from_list extract_enum tail in
            begin match tail with
            | { desc = lazy (AccessSpecifier CXXPrivate) } :: tail ->
                enums, tail
            | _ ->
                [], fields
            end
        | _ -> [], fields in
      let enums = StringMap.of_seq (List.to_seq enums) in
      let extract_simple_field
          (field : Clang.Lazy.Decl.t) : argument_decl option =
        match Lazy.force field.desc with
        | Field { name; qual_type; _ } ->
            Some (make_argument_decl name qual_type enums)
        | _ -> None in
      let arguments, fields =
        match name with
        | "AlignedAttr" ->
            let expr_type =
              Clang.Lazy.Type.make (lazy (Pointer (
                Clang.Lazy.Type.make (lazy (Record (
                  Clang.Lazy.Ast.identifier_name "Expr")))))) in
            [make_argument_decl "alignmentExpr" expr_type enums], fields
        | _ ->
            Clang.extract_prefix_from_list extract_simple_field fields in
      let annotated_fields =
        Clang.Lazy.Decl.annotate_access_specifier CXXPrivate fields in
      let public_fields = get_public_fields annotated_fields in
      let spelling = find_spelling public_fields in
      let reduced_name = get_reduced_attribute_name name in
      enums |> StringMap.iter (fun _ (enum_decl' : enum_decl) ->
        let constants =
          enum_decl'.constants |> List.map (fun constant ->
            constant.converted_name) in
        let result =
          H.elaborated Enum
            (H.enum (Clang.Ast.identifier_name enum_decl'.c_type_name)) in
        let nested_name_specifier = [
          namespace_clang;
          TypeSpec (H.record (Clang.Ast.identifier_name name))] in
        let value = "value" in
        let parameter =
          H.parameter (H.typedef (Clang.Ast.identifier_name enum_decl'.name
            ~nested_name_specifier)) value in
        let nested_name_specifier =
          nested_name_specifier @
          [TypeSpec (H.record (Clang.Ast.identifier_name enum_decl'.name))] in
        let body = H.compound [H.switch (H.decl_of_string value) (H.compound
          (enum_decl'.constants |> List.concat_map
          (fun (constant : enum_constant) ->
            Option.fold
              (find_version_constraint versions constant.converted_name)
              ~none:Fun.id ~some:restrict_statement_version
              [H.case (H.decl_of_string ~nested_name_specifier
                constant.constant_name)
                (H.return
                  (Some (H.decl_of_string constant.converted_name)))])));
          H.return (Some
            (H.decl_of_string (List.hd enum_decl'.constants).converted_name))] in
        let convert_function =
          H.function_decl (Clang.Ast.function_decl (Clang.Ast.function_type
            ~parameters:(Clang.Ast.parameters [parameter]) result)
            (IdentifierName enum_decl'.c_type_conversion_function)
            ~body) in
        let decls =
          Option.fold (find_version_constraint versions reduced_name)
            ~none:Fun.id ~some:restrict_decl_version [convert_function] in
        context.protos <-
          H.enum_decl enum_decl'.c_type_name (List.map H.enum_constant constants) ::
          context.protos;
        context.decls <- List.rev_append decls context.decls);
      if arguments <> [] || spelling <> None then
        let public_methods = enumerate_methods public_fields in
        generate_attribute context versions name reduced_name public_methods
          spelling arguments
  | _ -> ()

let do_namespace context versions (decl : Clang.Lazy.Decl.t) =
  match Lazy.force decl.desc with
  | Namespace { name = "clang"; declarations; _ } ->
      List.iter (do_decl context versions) declarations
  | _ ->
      ()

let rec partition_map_aux (accu_some : 'b list) (accu_none : 'a list)
    (f : 'a -> 'b option) (l : 'a list) : 'b list * 'a list =
  match l with
  | [] -> List.rev accu_some, List.rev accu_none
  | head :: tail ->
      match f head with
      | None -> partition_map_aux accu_some (head :: accu_none) f tail
      | Some image -> partition_map_aux (image :: accu_some) accu_none f tail

let partition_map f l =
  partition_map_aux [] [] f l

let filter_singleton (key, (desc : argument_desc)) =
  match desc.attributes with
  | [attribute] -> Some (key, attribute, desc)
  | _ -> None

let data = "data"

let callback = "callback"

let generate_code context versions argument type_name_attr ty
    (argument_desc : argument_desc) =
  let parameter_list = [parameter_cursor] in
  let parameter_list, result =
    if argument_desc.type_info.multiple then
      parameter_list @ [H.parameter (H.pointer (H.function_type H.void
        ~parameters:(Clang.Ast.parameters [
          H.parameter argument_desc.type_info.interface_type "";
          H.parameter (H.pointer H.void) ""]))) callback;
        H.parameter (H.pointer H.void) data],
      H.void
    else
      parameter_list, argument_desc.type_info.interface_type in
  let parameters = Clang.Ast.parameters parameter_list in
  let name =
    Printf.sprintf "clang_ext_%s_get%s" type_name_attr
      (String.capitalize_ascii argument) in
  argument_desc.getter_name_ref := name;
  let make_attribute_cast attribute =
    let param suffix =
      H.call (H.arrow (H.decl_of_string qual_attr) (FieldName
        (Clang.Ast.node
          (Clang.Ast.identifier_name (attribute.getter ^ suffix))))) [] in
    cast attr qual_attr attribute.name
      (if argument_desc.type_info.multiple then
         let iter = "iter" in
         let item = "item" in
         H.for_
           ~init:(H.decl [H.var H.auto iter ~init:(param "_begin")])
           ~cond:(H.binary_operator (H.decl_of_string iter) NE (param "_end"))
           ~inc:(H.expr (H.unary_operator PreInc (H.decl_of_string iter)))
           (H.compound [
             H.decl [H.var (H.lvalue_reference (H.const H.auto)) item
               ~init:(H.unary_operator Deref (H.decl_of_string iter))];
             H.expr (H.call (H.decl_of_string callback)
               [argument_desc.type_info.access (H.decl_of_string item);
                 H.decl_of_string data])])
       else
         H.return
           (Some (argument_desc.type_info.access (param "")))) in
  let make_attribute decorate attribute : Clang.Stmt.t list =
    let body = decorate (make_attribute_cast attribute) in
    let full_argument_name =
      Printf.sprintf "%s:%s" attribute.reduced_name argument in
    Option.fold (max
        (find_version_constraint versions full_argument_name)
        (find_version_constraint versions attribute.reduced_name))
      ~none:Fun.id ~some:restrict_statement_version body in
  let switch =
    match argument_desc.attributes with
    | [attribute] -> make_attribute (fun x -> [x]) attribute
    | attributes ->
        let make_case (attribute : argument_attribute) =
          make_attribute (fun x ->
          [H.case (H.decl_of_string (get_reduced_attribute_name attribute.name)
                 ~nested_name_specifier:[
                   namespace_clang;
                   Clang.Ast.NamespaceName "attr"])
             x; H.break]) attribute in
        let cases =
          H.compound
            (List.concat_map make_case attributes @ [H.default H.null_stmt]) in
        [H.switch (H.call (H.arrow (H.decl_of_string attr)
          (H.field_name (Clang.Ast.identifier_name "getKind"))) [])
          cases] in
  let list = [get_cursor_attr] @
    switch @
    if argument_desc.type_info.multiple then
      []
    else
      [H.return (Some argument_desc.type_info.default)] in
  let fun_decl =
    Clang.Ast.function_decl (Clang.Ast.function_type ~parameters result)
      (IdentifierName name) ~body:(H.compound list) in
  add_fun_decl context fun_decl

let tool_name = "generate_attrs"

let parse_ml_file filename =
  let channel = open_in filename in
  Fun.protect (fun () ->
    let lexbuf = Lexing.from_channel channel in
    Ppxlib.Parse.implementation lexbuf)
    ~finally:(fun () -> close_in channel)

let find_attributes_among_clang_versions dir : (int * int) StringMap.t =
  let bindings =
    Sys.readdir dir |> Array.to_list |> List.filter_map (fun filename ->
      let full_filename = Filename.concat dir filename in
      match List.map int_of_string_opt (String.split_on_char '.' filename) with
      | [Some major; Some minor; Some subminor]
        when Sys.is_directory full_filename ->
          let bindings =
            parse_ml_file
              (Filename.concat full_filename "clang__bindings.ml") in
          let minor =
            if major = 11 then
              match minor, subminor with
              | 1, 0 | 0, 1 -> 1
              | _ -> 0
            else if major >= 4 then
              0
            else
              minor in
          Some ((major, minor), bindings)
      | _ -> None) in
  let bindings =
    List.sort (fun (a, _) (b, _) -> compare a b) bindings in
  List.fold_left (fun previous (version, bindings) ->
    let attributes =
      bindings |> List.find_map (fun (item : Ppxlib.structure_item) ->
        match item.pstr_desc with
        | Pstr_type (Recursive, type_declarations) ->
            begin
              type_declarations |> List.find_map (
              fun (decl : Ppxlib.type_declaration) ->
                if decl.ptype_name.txt = "clang_ext_attrkind" then
                  match decl.ptype_kind with
                  | Ptype_variant constructors ->
                      Some (constructors |> List.map (fun
                        (constructor : Ppxlib.constructor_declaration) ->
                        constructor.pcd_name.txt))
                  | _ -> assert false
                else None)
            end
        | _ -> None) |> Option.get in
    List.fold_left (fun versions attribute ->
      let version =
        match StringMap.find_opt attribute previous with
        | Some version -> version
        | _ -> version in
      StringMap.add attribute version versions) StringMap.empty attributes)
    StringMap.empty bindings

let main cflags llvm_config prefix =
  let versions = find_attributes_among_clang_versions prefix in
  let versions =
    versions |>
    (* min and max arguments were unsigned int *)
    StringMap.add "AMDGPUWavesPerEU" (9, 0) |>
    StringMap.add "AMDGPUFlatWorkGroupSize" (9, 0) |>
    (* no argument strict nor priority *)
    StringMap.add "Availability" (9, 0) |>
    (* module was not IdentifierInfo *)
    StringMap.add "Ownership" (3, 7) |>
    (* getMinBlocks were int *)
    StringMap.add "CUDALaunchBounds" (3, 7) |>
    (* no argument replacement *)
    StringMap.add "Deprecated" (3, 9) |>
    (* no message *)
    StringMap.add "WarnUnusedResult" (10, 0) |>
    (* no isLiteralLabel *)
    StringMap.add "AsmLabel" (10, 0) |>
    (* no args *)
    StringMap.add "AssertCapability" (6, 0) |>
    (* no features_str_length *)
    StringMap.add "Target" (3, 8) |>
    (* fields are misnamed *)
    StringMap.add "CallableWhen" (3, 5) |>
    (* some constants are missing *)
    StringMap.add "LoopHint" (10, 0) |>
    (* some spelling are missing *)
    StringMap.add "clang_ext_Aligned_C2x_gnu_aligned" (11, 0) |>
    StringMap.add "clang_ext_AlwaysInline_C2x_gnu_always_inline" (11, 0) |>
    StringMap.add "clang_ext_MipsLongCall_C2x_gnu_long_call" (11, 0) |>
    StringMap.add "clang_ext_MipsLongCall_C2x_gnu_far" (11, 0) |>
    StringMap.add "clang_ext_MipsShortCall_C2x_gnu_short_call" (11, 0) |>
    StringMap.add "clang_ext_MipsShortCall_C2x_gnu_near" (11, 0) |>
    StringMap.add "clang_ext_Restrict_C2x_gnu_malloc" (11, 0) |>
    StringMap.add "clang_ext_Section_C2x_gnu_section" (11, 0) |>
    StringMap.add "clang_ext_Unused_C2x_gnu_unused" (11, 0) |>
    StringMap.add "clang_ext_WarnUnusedResult_C2x_gnu_warn_unused_result"
      (11, 0) |>
    StringMap.add
        "clang_ext_ArgumentWithTypeTag_CXX11_clang_argument_with_type_tag"
        (7, 0) |>
    StringMap.add
        "clang_ext_ArgumentWithTypeTag_C2x_clang_argument_with_type_tag"
        (7, 0) |>
    StringMap.add
        "clang_ext_ArgumentWithTypeTag_CXX11_clang_pointer_with_type_tag"
        (7, 0) |>
    StringMap.add
        "clang_ext_ArgumentWithTypeTag_C2x_clang_pointer_with_type_tag"
        (7, 0) |>
    StringMap.add
        "clang_ext_Ownership_C2x_clang_ownership_holds"
        (7, 0) |>
    StringMap.add
        "clang_ext_Ownership_C2x_clang_ownership_returns"
        (7, 0) |>
    StringMap.add
        "clang_ext_Ownership_C2x_clang_ownership_takes"
        (7, 0) |>
    StringMap.add
        "clang_ext_XRayInstrument_C2x_clang_xray_always_instrument"
        (7, 0) |>
    StringMap.add
        "clang_ext_XRayInstrument_C2x_clang_xray_never_instrument"
        (7, 0) |>
    StringMap.add
        "clang_ext_ReleaseCapability_CXX11_clang_unlock_function"
        (6, 0) |>
    StringMap.add
    "clang_ext_RequiresCapability_CXX11_clang_exclusive_locks_required"
        (6, 0) |>
    StringMap.add
    "clang_ext_RequiresCapability_CXX11_clang_shared_locks_required"
        (6, 0) |>
    StringMap.add "clang_ext_Unused_C2x_maybe_unused" (6, 0) |>
    StringMap.add
        "clang_ext_Ownership_CXX11_clang_ownership_holds"
        (6, 0) |>
    StringMap.add
        "clang_ext_Ownership_CXX11_clang_ownership_returns"
        (6, 0) |>
    StringMap.add
        "clang_ext_Ownership_CXX11_clang_ownership_takes"
        (6, 0) |>
    (* some constants are missing *)
    StringMap.add
        "clang_ext_OMPAllocateDeclAttr_AllocatorTypeTy_OMPNullMemAlloc"
        (11, 0) |>
    (* getCaptureKind becomes getCaptureKindVal *)
    StringMap.add "OMPCaptureKind" (11, 0) |>
    (* getGuid becomes getGuidDecl *)
    StringMap.add "Uuid" (11, 0) |>
    (* OMPDeclareVariant uses OMPTraitInfo *)
    StringMap.add "OMPDeclareVariant" (11, 0) |>
    (* Spelling is missing *)
    StringMap.add "clang_ext_OpenCLConstantAddressSpace_spelling" (10, 0) |>
    StringMap.add "clang_ext_OpenCLGenericAddressSpace_spelling" (10, 0) |>
    StringMap.add "clang_ext_OpenCLGlobalAddressSpace_spelling" (10, 0) |>
    StringMap.add "clang_ext_OpenCLLocalAddressSpace_spelling" (10, 0) |>
    StringMap.add "clang_ext_OpenCLPrivateAddressSpace_spelling" (10, 0) |>
    StringMap.add "clang_ext_PassObjectSize_spelling" (9, 0) |>
    StringMap.add "clang_ext_Unused_spelling" (3, 9) |>
    (* args is missing *)
    StringMap.add "Annotate:args" (12, 0) |>
    StringMap.add "Annotate:args_Size" (12, 0) in
  let command_line_args, _llvm_version =
    Stubgen_common.prepare_clang_options cflags llvm_config in
  let tu =
    Clang.Lazy.Ast.parse_string ~filename:"string.cpp" ~command_line_args {|
      #include <clang/AST/Attr.h>
    |} in
  Clang.Lazy.Ast.format_diagnostics Clang.warning_or_error Format.err_formatter
    tu;
  assert (not (Clang.Lazy.Ast.has_severity Clang.error tu));
  let context = {
    argument_table = StringHashtbl.create 17;
    constructors = [];
    decls = [];
    protos = [];
    attributes = [];
  } in
  List.iter (do_namespace context versions) (Lazy.force tu.desc).items;
  context.argument_table |> StringHashtbl.iter (fun argument types ->
    let singletons, multiples =
      partition_map filter_singleton (List.of_seq (TypeHashtbl.to_seq types)) in
    singletons |> List.iter (fun (ty, (singleton : argument_attribute), desc) ->
      generate_code context versions argument singleton.name
        ty desc);
    match multiples with
    | [] -> ()
    | [(key, argument_desc)] ->
        generate_code context versions argument "Attrs" key argument_desc
    | _ ->
        multiples |> List.iter (fun (key, (argument_desc : argument_desc)) ->
          let type_attr_name =
            (List.hd (List.rev argument_desc.attributes)).name in
          generate_code context versions argument type_attr_name key
            argument_desc));
  let other =
    Ppxlib.Ast_helper.Type.constructor (Metapp.mkloc "Other")
      ~args:(Pcstr_tuple [[%type: Clang__bindings.clang_ext_attrkind]]) in
  let groups = [
    "AMD"; "AVR"; "AnyX86"; "CPU"; "CUDA"; "IB"; "NS"; "OMP"; "OS"; "ObjC";
    "OpenCL"; "PragmaClang"; "Swift"; "WebAssembly"] in
  let groups = List.map (fun group -> group, ref []) groups in
  let constructors = List.rev (other :: context.constructors) in
  let constructors = constructors |> List.filter
    (fun (constructor : Ppxlib.constructor_declaration) ->
      match List.find_map (fun (prefix, list) -> Option.map (fun suffix -> suffix, list) (Stubgen_common.String_utils.remove_prefix prefix constructor.pcd_name.txt)) groups with
      | None -> true
      | Some (suffix, list) ->
          list := { constructor with pcd_name = { constructor.pcd_name with txt = suffix }} :: !list;
          false) in
  let groups =
    groups |> List.filter_map (fun (prefix, list) ->
      let list = !list in
      match list with
      | [] -> None
      | _ -> Some (prefix, List.rev list)) in
  let parameters =
    [[%type: 'expr]; [%type: 'decl]; [%type: 'qual_type];
      [%type: 'declaration_name]; [%type: 'omp_trait_info]] in
  let constructors =
    (groups |> List.map (fun (prefix, _) ->
      Ppxlib.Ast_helper.Type.constructor (Metapp.mkloc prefix)
        ~args:(Pcstr_tuple [Ppxlib.Ast_helper.Typ.constr (Metapp.mklid (Stubgen_common.uncamelcase prefix))
          parameters])))
    @ constructors in
  let make_type_declaration name list =
    Ppxlib.Ast_helper.Type.mk (Metapp.mkloc name)
      ~kind:(Ptype_variant list)
      ~params:(List.map (fun ty -> ty, Ppxlib.Asttypes.Invariant) parameters)
      ~attrs:[Metapp.Attr.mk (Metapp.mkloc "deriving") (PStr [%str refl])] in
  let ty =
    make_type_declaration "t" constructors in
  let type_decl =
    ((groups |> List.map (fun (prefix, list) ->
      make_type_declaration (Stubgen_common.uncamelcase prefix) list))
    @ [ty]) |> List.map (fun decl -> Ppxlib.Ast_helper.Str.type_ Recursive [decl]) in
  let cases =
    context.attributes |> List.map (
    fun (attribute : ocaml_attribute) ->
      let lid = Metapp.mklid attribute.name in
      let attrs =
        match find_version_constraint versions attribute.name with
        | None -> []
        | Some (major, minor) ->
            let make_condition_attr condition =
              Metapp.Attr.mk (Metapp.mkloc "if") (PStr [%str [%meta
                Metapp.Exp.of_bool [%e condition]]]) in
            if major = 3 || major = 11 then
              [make_condition_attr
                 [%expr (Clangml_config.equivalent_version.major,
                   Clangml_config.equivalent_version.minor) >=
                   ([%e Metapp.Exp.of_int major],
                     [%e Metapp.Exp.of_int minor])]]
            else if major >= 4 then
              [make_condition_attr
                 [%expr Clangml_config.equivalent_version.major >=
                   [%e Metapp.Exp.of_int major]]]
            else
              assert false in
      let pattern = Ppxlib.Ast_helper.Pat.construct lid None ~attrs in
      let expr =
        let args =
          attribute.arguments |> List.map (fun (argument : ocaml_argument) ->
            let getter_name =
              !(argument.getter_name_ref) |>
              Stubgen_common.option_apply
                (Stubgen_common.String_utils.remove_prefix ~prefix:"clang_") |>
              Stubgen_common.uncamelcase in
            let converter =
              match argument.type_conversion with
              | NoConversion -> None
              | Expr -> Some [%expr expr_of_cxcursor]
              | Decl -> Some [%expr decl_of_cxcursor]
              | TypeLoc -> Some [%expr of_type_loc]
              | OMPTraitInfo -> Some [%expr omp_trait_info_of_cxcursor]
              | DeclarationName ->
                 Some [%expr convert_declaration_name] in
            let getter =
              let ident =
                Ppxlib.Ast_helper.Exp.ident
                  (Metapp.mklid ~prefix:(Lident "Clang__bindings")
                     getter_name) in
              if argument.multiple then
                let e = [%expr Clang__utils.list_of_iter ([%e ident] cursor)] in
                match converter with
                | None -> e
                | Some converter -> [%expr [%e e] |> List.map [%e converter]]
              else
                let e = [%expr [%e ident] cursor] in
                match converter with
                | None -> e
                | Some converter -> [%expr [%e converter] [%e e]] in
            Metapp.mklid argument.name, getter) in
        let args =
          match args with
          | [(_, arg)] -> arg
          | _ -> Ppxlib.Ast_helper.Exp.record args None in
        let construct =
          match List.find_map (fun (prefix, _) -> Option.map (fun suffix -> prefix, suffix) (Stubgen_common.String_utils.remove_prefix prefix attribute.name)) groups with
          | Some (prefix, suffix) ->
              (fun args -> Ppxlib.Ast_helper.Exp.construct (Metapp.mklid prefix) (Some (Ppxlib.Ast_helper.Exp.construct (Metapp.mklid suffix) args)))
          | None -> Ppxlib.Ast_helper.Exp.construct lid in
        construct (Some args) in
      Ppxlib.Ast_helper.Exp.case pattern expr) in
  let cases = Ppxlib.Ast_helper.Exp.case [%pat? other] [%expr Other other] :: cases in
  let convert =
    let pattern_matching =
      Ppxlib.Ast_helper.Exp.match_ [%expr Clang__bindings.ext_attr_get_kind cursor]
        (List.rev cases) in
    [%stri
       [%%meta (new Metapp.filter)#structure_item [%stri
       let convert cursor expr_of_cxcursor decl_of_cxcursor of_type_loc
             convert_declaration_name omp_trait_info_of_cxcursor =
         [%e pattern_matching]]]] in
  let chan = open_out (prefix ^ "attributes.ml") in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
    Stubgen_common.output_warning_ml chan tool_name;
    let fmt = Format.formatter_of_out_channel chan in
    Format.fprintf fmt "%a@." Ppxlib.Pprintast.structure
      ([%str
          [%%metapackage "metapp"]
          [%%metadir "config/.clangml_config.objs/byte"]] @
        type_decl @ [convert]));
  let chan = open_out (prefix ^ "libclang_extensions_attrs.inc") in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
    Stubgen_common.output_warning_c chan tool_name;
    let fmt = Format.formatter_of_out_channel chan in
    Format.fprintf fmt "%a@." Clang.Printer.decls (List.rev context.decls));
  let chan = open_out (prefix ^ "libclang_extensions_attrs_headers.inc") in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
    Stubgen_common.output_warning_c chan tool_name;
    let fmt = Format.formatter_of_out_channel chan in
    Format.fprintf fmt "%a@." Clang.Printer.decls (List.rev context.protos))

let info =
  let doc = "generate stubs for ClangML attributes" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info tool_name ~doc ~exits:Cmdliner.Term.default_exits ~man

let () =
  Cmdliner.Term.exit (Cmdliner.Term.eval (Stubgen_common.options main, info))
