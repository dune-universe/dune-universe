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

let has_suffix ~suffix s =
  let suffix_len = String.length suffix in
  let len = String.length s in
  suffix_len <= len && String.sub s (len - suffix_len) suffix_len = suffix

let elaborated ?nested_name_specifier keyword named_type =
  Clang.Type.make (Elaborated { keyword; nested_name_specifier; named_type })

let function_decl fun_decl =
  Clang.Ast.node (Clang.Ast.Function fun_decl)

let function_type ?calling_conv ?parameters ?exception_spec result =
  Clang.Type.make (FunctionType (Clang.Ast.function_type
    ?calling_conv ?parameters ?exception_spec result))

let record ident_ref =
  Clang.Type.make (Record ident_ref)

let typedef name = Clang.Type.make (Typedef name)

let enum name = Clang.Type.make (Enum name)

let cxcursor = typedef (Clang.Ast.identifier_name "CXCursor")

let pointer ty = Clang.Type.make (Pointer ty)

let void = Clang.Type.make (BuiltinType Void)

let int = Clang.Type.make (BuiltinType Int)

let unsigned_int = Clang.Type.make (BuiltinType UInt)

let bool = Clang.Type.make (BuiltinType Bool)

let auto = Clang.Type.make Auto

let callback = "callback"

let call callee args = Clang.Ast.node (Clang.Ast.Call { callee; args })

let decl_ref decl_ref = Clang.Ast.node (Clang.Ast.DeclRef decl_ref)

let decl_of_string ?nested_name_specifier s =
  decl_ref (Clang.Ast.identifier_name ?nested_name_specifier s)

let string s =
  Clang.Ast.node (Clang.Ast.StringLiteral (Clang.Ast.literal_of_string s))

let const_int i = Clang.Ast.node (Clang.Ast.IntegerLiteral (Int i))

let const_bool b = Clang.Ast.node (Clang.Ast.BoolLiteral b)

let compound list = Clang.Ast.node (Clang.Ast.Compound list)

let decl list = Clang.Ast.node (Clang.Ast.Decl list)

let expr e = Clang.Ast.node (Clang.Ast.Expr e)

let unary_operator kind operand =
  Clang.Ast.node (Clang.Ast.UnaryOperator { kind; operand })

let binary_operator lhs kind rhs =
  Clang.Ast.node (Clang.Ast.BinaryOperator { lhs; kind; rhs })

let member ?base ?(arrow = false) field =
  Clang.Ast.node (Clang.Ast.Member { base; arrow; field })

let arrow base field =
  member ~base ~arrow:true field

let field_name s =
  Clang.Ast.FieldName (Clang.Ast.node s)

let field_of_string s =
  field_name (Clang.Ast.identifier_name s)

let var ~init qual_type name =
  Clang.Ast.node (Clang.Ast.Var (Clang.Ast.var ~var_init:init name qual_type))

let directive directive =
  Clang.Ast.node (Clang.Ast.Directive directive)

let const (qual_type : Clang.Type.t) =
  { qual_type with const = true }

let lvalue_reference qual_type =
  Clang.Type.make (LValueReference qual_type)

let rvalue_reference qual_type =
  Clang.Type.make (RValueReference qual_type)

let return e =
  Clang.Ast.node (Clang.Ast.Return e)

let array_subscript base index =
  Clang.Ast.node (Clang.Ast.ArraySubscript { base; index })

let conditional_operator cond ?then_branch else_branch =
  Clang.Ast.node (
    Clang.Ast.ConditionalOperator { cond; then_branch; else_branch })

let parameter qual_type name =
  Clang.Ast.node (Clang.Ast.parameter qual_type name)

let switch ?init ?condition_variable cond body =
  Clang.Ast.node (Clang.Ast.Switch { init; condition_variable; cond; body })

let for_ ?init ?condition_variable ?cond ?inc body =
  Clang.Ast.node (Clang.Ast.For { init; condition_variable; cond; inc; body })

let for_range var range body =
  Clang.Ast.node (Clang.Ast.ForRange { var; range; body })

let case ?rhs lhs body =
  Clang.Ast.node (Clang.Ast.Case { lhs; rhs; body })

let default stmt =
  Clang.Ast.node (Clang.Ast.Default stmt)

let break =
  Clang.Ast.node (Clang.Ast.Break)

let null_stmt =
  Clang.Ast.node (Clang.Ast.Null)

let enum_decl ?(complete_definition = false) name constants =
  Clang.Ast.node (Clang.Ast.EnumDecl
    { name; constants; complete_definition; attributes = [] })

let enum_constant ?constant_init constant_name : Clang.Ast.enum_constant =
  Clang.Ast.node
    ({ constant_name; constant_init } : Clang.Ast.enum_constant_desc)

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
  | TypeLoc
  | DeclarationName

type type_info = {
    ocaml_type : Parsetree.core_type;
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
    lazy (call (decl_ref (Clang.Ast.identifier_name "getCursorTU"))
      [decl_ref (Clang.Ast.identifier_name "cursor")]) in
  match Lazy.force qual_type.desc with
  | Pointer { desc = lazy (Record { name = IdentifierName "Expr"})} ->
      { ocaml_type = [%type: 'expr];
        type_conversion = Expr;
        interface_type = cxcursor;
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "MakeCXCursor"))
            [e; Lazy.force get_cursor_tu]);
        default =
          call (decl_ref (Clang.Ast.identifier_name "MakeCXCursorInvalid"))
            [decl_ref (Clang.Ast.identifier_name "CXCursor_InvalidCode");
              Lazy.force get_cursor_tu]}
  | Record { name = IdentifierName "VersionTuple" } ->
      { ocaml_type = [%type: Clang__bindings.clang_ext_versiontuple];
        type_conversion = NoConversion;
        interface_type =
          elaborated Struct
            (record (Clang.Ast.identifier_name "clang_ext_VersionTuple"));
        multiple = false;
        access = (fun e ->
          call (decl_ref
              (Clang.Ast.identifier_name "makeVersionTuple")) [e]);
        default = decl_ref (Clang.Ast.identifier_name "zeroVersionTuple")}
  | Record { name = IdentifierName "StringRef" }
  | Pointer { desc = lazy (BuiltinType Char_S) } ->
      { ocaml_type = [%type: string];
        type_conversion = NoConversion;
        interface_type =
          typedef (Clang.Ast.identifier_name "CXString");
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createDup"))
            [e]);
        default =
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createRef"))
            [string ""] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "IdentifierInfo" }) } ->
      { ocaml_type = [%type: string];
        type_conversion = NoConversion;
        interface_type =
          typedef (Clang.Ast.identifier_name "CXString");
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createDup"))
            [call (arrow e (field_name
              (Clang.Ast.identifier_name "getName"))) []]);
        default =
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createRef"))
            [string ""] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "FunctionDecl" }) } ->
      { ocaml_type = [%type: 'declaration_name];
        type_conversion = DeclarationName;
        interface_type =
          elaborated Struct
            (record (Clang.Ast.identifier_name "clang_ext_DeclarationName"));
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "MakeDeclarationName"))
            [call (arrow e (field_name
              (Clang.Ast.identifier_name "getDeclName"))) [];
             Lazy.force get_cursor_tu]);
        default =
          call (decl_ref
                  (Clang.Ast.identifier_name "MakeDeclarationNameInvalid"))
            [Lazy.force get_cursor_tu] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "TypeSourceInfo" }) } ->
      { ocaml_type = [%type: 'qual_type];
        type_conversion = TypeLoc;
        interface_type =
          elaborated Struct
            (record (Clang.Ast.identifier_name "clang_ext_TypeLoc"));
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "MakeTypeLoc"))
            [call (arrow e (field_name
              (Clang.Ast.identifier_name "getTypeLoc"))) [];
             Lazy.force get_cursor_tu]);
        default =
          call (decl_ref
                  (Clang.Ast.identifier_name "MakeTypeLocInvalid"))
            [Lazy.force get_cursor_tu] }
  | Record { name = IdentifierName "ParamIdx" } ->
      { ocaml_type =  [%type: int];
        type_conversion = NoConversion;
        interface_type = unsigned_int;
        multiple = false;
        access =
          (fun e -> call (decl_of_string "unsigned_int_of_ParamIdx") [e]);
        default = const_int 0; }
  | BuiltinType Int ->
      { ocaml_type =  [%type: int];
        type_conversion = NoConversion;
        interface_type = int;
        multiple = false;
        access = Fun.id;
        default = const_int 0; }
  | BuiltinType UInt ->
      { ocaml_type =  [%type: int];
        type_conversion = NoConversion;
        interface_type = unsigned_int;
        multiple = false;
        access = Fun.id;
        default = const_int 0; }
  | BuiltinType Bool ->
      { ocaml_type =  [%type: bool];
        type_conversion = NoConversion;
        interface_type = bool;
        multiple = false;
        access = Fun.id;
        default = const_bool false; }
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
            Ast_helper.Typ.constr
              (Metapp.mklid ~prefix:(Lident clang__bindings)
                 enum_decl.ocaml_type_name) [];
            type_conversion = NoConversion;
            interface_type =
            elaborated Enum
              (enum (Clang.Ast.identifier_name enum_decl.c_type_name));
            multiple = false;
            access = (fun e ->
              call (decl_of_string enum_decl.c_type_conversion_function) [e]);
            default =
            decl_of_string (List.hd enum_decl.constants).converted_name;
          }
      | None -> assert false
      end
  | _ ->
      Format.eprintf "Unsupported type %a@."
        (Refl.pp [%refl:Clang.Lazy.Ast.qual_type] []) qual_type;
      { ocaml_type =  [%type: bool];
        type_conversion = NoConversion;
        interface_type = bool;
        multiple = false;
        access = Fun.id;
        default = const_bool false; }

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
    ty : Parsetree.core_type;
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
    mutable constructors : Parsetree.constructor_declaration list;
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

type annotated_field = Clang.Ast.cxx_access_specifier * Clang.Lazy.Ast.decl

let annotate_access_specifier
    (default_specifier : Clang.Ast.cxx_access_specifier)
    (fields : Clang.Lazy.Ast.decl list) : annotated_field list =
  let annotate_field (specifier, rev) (field : Clang.Lazy.Ast.decl) =
    match Lazy.force field.desc with
    | AccessSpecifier specifier -> (specifier, rev)
    | _ -> (specifier, (specifier, field) :: rev) in
  let _specifier, rev =
    List.fold_left annotate_field (default_specifier, []) fields in
  List.rev rev

let get_constant_names
    (constants : Clang.Lazy.Ast.enum_constant list) : string list =
   constants |>
   List.map (fun ({ desc = lazy constant } : Clang.Lazy.Ast.enum_constant) ->
     constant.constant_name)

let find_spelling (fields : annotated_field list) : string list option =
  let get_spelling ((specifier, field) : annotated_field) =
    match specifier with
    | CXXPublic ->
        begin match Lazy.force field.desc with
        | EnumDecl { name = "Spelling"; constants; _ } ->
            Some (get_constant_names constants)
        | _ -> None
        end
    | _ -> None in
  List.find_map get_spelling fields

let enumerate_public_methods (fields : annotated_field list) :
    (string * Clang.Lazy.Type.t) StringHashtbl.t =
  let table = StringHashtbl.create 17 in
  let add_field ((specifier, field) : annotated_field) =
    match specifier with
    | CXXPublic ->
        begin match Lazy.force field.desc with
        | CXXMethod {
          function_decl = { name = IdentifierName name; function_type }; _ } ->
            let normalized_name =
              String.lowercase name |>
              Stubgen_common.option_apply
                (Stubgen_common.string_remove_prefix ~prefix:"get") |>
              Stubgen_common.option_apply
                (Stubgen_common.string_remove_suffix ~suffix:"loc") in
            StringHashtbl.add table normalized_name (name, function_type.result)
        | _ -> ()
        end
    | _ -> () in
  fields |> List.iter add_field;
  table

let get_reduced_attribute_name attribute =
  Option.get (Stubgen_common.string_remove_suffix attribute ~suffix:"Attr")

let remove_trailing_underscore argument =
  Stubgen_common.option_apply
    (Stubgen_common.string_remove_suffix ~suffix:"_") argument

let get_type_spelling_name name =
  Printf.sprintf "clang_ext_%s_spelling" name

let cursor = "cursor"

let attr = "attr"

let qual_attr = "qual_attr"

let parameter_cursor = parameter cxcursor cursor

let get_cursor_attr =
  Clang.Ast.node (Clang.Ast.Decl [
    var auto attr ~init:(call
      (decl_ref (Clang.Ast.identifier_name "GetCursorAttr"))
      [decl_ref (Clang.Ast.identifier_name cursor)])])

let cast attr qual_attr class_name body =
  Clang.Ast.node (Clang.Ast.if_
        ~condition_variable:(Clang.Ast.node (Clang.Ast.var
          qual_attr auto
          ~var_init:(call
             (Clang.Ast.node (Clang.Ast.DeclRef
               (Clang.Ast.identifier_name "dyn_cast_or_null"
                 ~nested_name_specifier:[Clang.Ast.NamespaceName "llvm"]
                 ~template_arguments:[
                   Type (Clang.Type.make (Clang.Ast.Record
                     (Clang.Ast.identifier_name class_name
                     ~nested_name_specifier:[
                       Clang.Ast.NamespaceName "clang"])))])))
             [decl_ref (Clang.Ast.identifier_name attr)])))
        (Clang.Ast.node (Clang.Ast.DeclRef
          (Clang.Ast.identifier_name qual_attr))) body)

let namespace_clang = Clang.Ast.NamespaceName "clang"

let add_fun_decl context fun_decl =
  context.decls <- function_decl fun_decl :: context.decls;
  context.protos <-
    function_decl { fun_decl with body = None } :: context.protos

let unkeyword name =
  match name with
  | "type"
  | "module" -> name ^ "_"
  | _ -> name

let restrict_decl_version (major, minor) body =
  directive (Ifndef (
    Printf.sprintf "LLVM_VERSION_BEFORE_%d_%d_0" major minor)) ::
  body @ [directive Endif]

let restrict_statement_version (major, minor) body =
  decl [directive (Ifndef (
    Printf.sprintf "LLVM_VERSION_BEFORE_%d_%d_0" major minor))] ::
  body @ [decl [directive Endif]]

let find_version_constraint versions attribute =
  match StringMap.find_opt attribute versions with
  | None ->
      prerr_endline attribute;
      assert false
  | Some (3, 4) -> None
  | result -> result

let generate_attribute context versions name reduced_name public_methods
    spelling (arguments : argument_decl list) =
  let arguments =
    arguments |> List.map @@ fun (argument : argument_decl) ->
      let arg_name = remove_trailing_underscore argument.name in
      let argument = { argument with name = arg_name } in
      match
        StringHashtbl.find_opt public_methods (String.lowercase arg_name)
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
    if has_suffix ~suffix:"_Size" argument.name
        || has_suffix ~suffix:"Length" argument.name then
      None
    else
      let name =
        argument.name |>
        Stubgen_common.option_apply
          (Stubgen_common.string_remove_suffix ~suffix:"Param") |>
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
        ty = Ast_helper.Typ.constr
          (Metapp.mklid ~prefix:(Lident clang__bindings)
             (String.lowercase type_spelling_name)) [];
        type_conversion = NoConversion;
        multiple = false;
        getter_name_ref = ref spelling_getter_name } :: arguments in
  let args : Parsetree.constructor_arguments =
    match arguments with
    | [argument] ->
        Pcstr_tuple [argument.ty]
    | _ ->
        Pcstr_record (arguments |> List.map (
        fun (argument : ocaml_argument) ->
          Ast_helper.Type.field (Metapp.mkloc argument.name) argument.ty)) in
  let constructor =
    Ast_helper.Type.constructor (Metapp.mkloc reduced_name) ~args in
  context.constructors <- constructor :: context.constructors;
  spelling |> Option.iter (
  fun (spelling, type_spelling_name, spelling_getter_name) ->
    let constant_names = spelling |> List.map (fun constant ->
      constant, Printf.sprintf "clang_ext_%s_%s" reduced_name constant) in
    let last_constant = snd (List.hd (List.rev constant_names)) in
    let enum_constants = constant_names |> List.map (fun (_, constant) ->
      enum_constant constant) in
    let spelling_enum = enum_decl type_spelling_name enum_constants in
    let cases =
      constant_names |> List.concat_map (fun (orig, prefixed) ->
        let case =
          case (decl_ref (Clang.Ast.identifier_name orig
            ~nested_name_specifier:[
              namespace_clang;
              TypeSpec (record (Clang.Ast.identifier_name name));
              TypeSpec (record (Clang.Ast.identifier_name "Spelling"))]))
            (return (Some (decl_ref (Clang.Ast.identifier_name prefixed)))) in
        if orig = "SpellingNotCalculated" then
          restrict_statement_version (10, 0) [case]
        else
          [case]) in
    let switch =
      cast attr qual_attr name
        (switch (call (arrow (decl_ref (Clang.Ast.identifier_name qual_attr))
          (field_name (Clang.Ast.identifier_name "getSemanticSpelling"))) [])
          (compound cases)) in
    let return_default =
      return (Some (decl_ref (Clang.Ast.identifier_name last_constant))) in
    let list =
      restrict_statement_version (10, 0) [get_cursor_attr; switch]
      @ [return_default] in
    let result =
      elaborated Enum (enum (Clang.Ast.identifier_name type_spelling_name)) in
    let spelling_getter =
      Clang.Ast.function_decl (Clang.Ast.function_type
        ~parameters:(Clang.Ast.parameters [parameter_cursor]) result)
        (IdentifierName spelling_getter_name) ~body:(compound list) in
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
              ocaml_type_name = String.lowercase c_type_name;
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
      let annotated_fields = annotate_access_specifier CXXPrivate fields in
      let spelling = find_spelling annotated_fields in
      let reduced_name = get_reduced_attribute_name name in
      enums |> StringMap.iter (fun _ (enum_decl' : enum_decl) ->
        let constants =
          enum_decl'.constants |> List.map (fun constant ->
            constant.converted_name) in
        let result =
          elaborated Enum
            (enum (Clang.Ast.identifier_name enum_decl'.c_type_name)) in
        let nested_name_specifier = [
          namespace_clang;
          TypeSpec (record (Clang.Ast.identifier_name name))] in
        let value = "value" in
        let parameter =
          parameter (typedef (Clang.Ast.identifier_name enum_decl'.name
            ~nested_name_specifier)) value in
        let nested_name_specifier =
          nested_name_specifier @
          [TypeSpec (record (Clang.Ast.identifier_name enum_decl'.name))] in
        let body = compound [switch (decl_of_string value) (compound
          (enum_decl'.constants |> List.map
          (fun (constant : enum_constant) ->
            case (decl_of_string ~nested_name_specifier constant.constant_name)
              (return (Some (decl_of_string constant.converted_name))))));
          return (Some
            (decl_of_string (List.hd enum_decl'.constants).converted_name))] in
        let convert_function =
          function_decl (Clang.Ast.function_decl (Clang.Ast.function_type
            ~parameters:(Clang.Ast.parameters [parameter]) result)
            (IdentifierName enum_decl'.c_type_conversion_function)
            ~body) in
        let decls =
          Option.fold (find_version_constraint versions reduced_name)
            ~none:Fun.id ~some:restrict_decl_version [convert_function] in
        context.protos <-
          enum_decl enum_decl'.c_type_name (List.map enum_constant constants) ::
          context.protos;
        context.decls <- List.rev_append decls context.decls);
      if arguments <> [] || spelling <> None then
        let public_methods = enumerate_public_methods annotated_fields in
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

let generate_code context versions argument type_name_attr ty
    (argument_desc : argument_desc) =
  let parameter_list = [parameter_cursor] in
  let parameter_list, result =
    if argument_desc.type_info.multiple then
      parameter_list @ [parameter (pointer (function_type void
        ~parameters:(Clang.Ast.parameters [
          parameter argument_desc.type_info.interface_type "";
          parameter (pointer void) ""]))) callback;
        parameter (pointer void) data],
      void
    else
      parameter_list, argument_desc.type_info.interface_type in
  let parameters = Clang.Ast.parameters parameter_list in
  let name =
    Printf.sprintf "clang_ext_%s_get%s" type_name_attr
      (String.capitalize argument) in
  argument_desc.getter_name_ref := name;
  let make_attribute_cast attribute =
    let param suffix =
      call (arrow (decl_ref (Clang.Ast.identifier_name qual_attr)) (FieldName
        (Clang.Ast.node
          (Clang.Ast.identifier_name (attribute.getter ^ suffix))))) [] in
    cast attr qual_attr attribute.name
      (if argument_desc.type_info.multiple then
         let iter = "iter" in
         let item = "item" in
         for_
           ~init:(decl [var auto iter ~init:(param "_begin")])
           ~cond:(binary_operator (decl_of_string iter) NE (param "_end"))
           ~inc:(expr (unary_operator PreInc (decl_of_string iter)))
           (compound [
             decl [var (lvalue_reference (const auto)) item
               ~init:(unary_operator Deref (decl_of_string iter))];
             expr (call (decl_of_string callback)
               [argument_desc.type_info.access (decl_of_string item);
                 decl_of_string data])])
       else
         return
           (Some (argument_desc.type_info.access (param "")))) in
  let make_attribute decorate attribute : Clang.Stmt.t list =
    let body = decorate (make_attribute_cast attribute) in
    Option.fold (find_version_constraint versions attribute.reduced_name)
      ~none:Fun.id ~some:restrict_statement_version body in
  let switch =
    match argument_desc.attributes with
    | [attribute] -> make_attribute (fun x -> [x]) attribute
    | attributes ->
        let make_case (attribute : argument_attribute) =
          make_attribute (fun x ->
          [case (decl_ref
             (Clang.Ast.identifier_name
                (get_reduced_attribute_name attribute.name)
                 ~nested_name_specifier:[
                   namespace_clang;
                   Clang.Ast.NamespaceName "attr"]))
             x; break]) attribute in
        let cases =
          compound
            (List.concat_map make_case attributes @ [default null_stmt]) in
        [switch (call (arrow (decl_ref (Clang.Ast.identifier_name attr))
          (field_name (Clang.Ast.identifier_name "getKind"))) [])
          cases] in
  let list = [get_cursor_attr] @
    switch @
    if argument_desc.type_info.multiple then
      []
    else
      [return (Some argument_desc.type_info.default)] in
  let fun_decl =
    Clang.Ast.function_decl (Clang.Ast.function_type ~parameters result)
      (IdentifierName name) ~body:(compound list) in
  add_fun_decl context fun_decl

let tool_name = "generate_attrs"

let find_attributes_among_clang_versions dir : (int * int) StringMap.t =
  let bindings =
    Sys.readdir dir |> Array.to_list |> List.filter_map (fun filename ->
      let full_filename = Filename.concat dir filename in
      match List.map int_of_string_opt (String.split_on_char '.' filename) with
      | [Some major; Some minor; Some _subminor]
        when Sys.is_directory full_filename ->
          let bindings =
            Pparse.parse_implementation ~tool_name
              (Filename.concat full_filename "clang__bindings.ml") in
          let minor =
            if major >= 4 then
              0
            else
              minor in
          Some ((major, minor), bindings)
      | _ -> None) in
  let bindings =
    List.sort (fun (a, _) (b, _) -> compare a b) bindings in
  List.fold_left (fun previous (version, bindings) ->
    let attributes =
      bindings |> List.find_map (fun (item : Parsetree.structure_item) ->
        match item.pstr_desc with
        | Pstr_type (Recursive, type_declarations) ->
            begin
              type_declarations |> List.find_map (
              fun (decl : Parsetree.type_declaration) ->
                if decl.ptype_name.txt = "clang_ext_attrkind" then
                  match decl.ptype_kind with
                  | Ptype_variant constructors ->
                      Some (constructors |> List.map (fun
                        (constructor : Parsetree.constructor_declaration) ->
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
    StringMap.add "LoopHint" (10, 0) in
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
    Ast_helper.Type.constructor (Metapp.mkloc "Other")
      ~args:(Pcstr_tuple [[%type: Clang__bindings.clang_ext_attrkind]]) in
  let constructors = List.rev (other :: context.constructors) in
  let ty =
    Ast_helper.Type.mk (Metapp.mkloc "t")
      ~kind:(Ptype_variant constructors)
      ~params:[[%type: 'expr], Invariant; [%type: 'qual_type], Invariant;
        [%type: 'declaration_name], Invariant]
      ~attrs:[Metapp.Attr.mk (Metapp.mkloc "deriving") (PStr [%str refl])] in
  let type_decl =
    Ast_helper.Str.type_ Recursive [ty] in
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
            if major >= 4 then
              [make_condition_attr
                 [%expr Clangml_config.version.major >=
                   [%e Metapp.Exp.of_int major]]]
            else if major = 3 then
              [make_condition_attr
                 [%expr (Clangml_config.version.major,
                   Clangml_config.version.minor) >=
                   (3, [%e Metapp.Exp.of_int minor])]]
            else
              assert false in
      let pattern = Ast_helper.Pat.construct lid None ~attrs in
      let expr =
        let args =
          attribute.arguments |> List.map (fun (argument : ocaml_argument) ->
            let getter_name =
              !(argument.getter_name_ref) |>
              Stubgen_common.option_apply
                (Stubgen_common.string_remove_prefix ~prefix:"clang_") |>
              Stubgen_common.uncamelcase in
            let converter =
              match argument.type_conversion with
              | NoConversion -> None
              | Expr -> Some [%expr expr_of_cxcursor]
              | TypeLoc -> Some [%expr of_type_loc]
              | DeclarationName ->
                 Some [%expr convert_declaration_name] in
            let getter =
              let ident =
                Ast_helper.Exp.ident
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
          | _ -> Ast_helper.Exp.record args None in
        Ast_helper.Exp.construct lid (Some args) in
      Ast_helper.Exp.case pattern expr) in
  let cases = Ast_helper.Exp.case [%pat? other] [%expr Other other] :: cases in
  let convert =
    let pattern_matching =
      Ast_helper.Exp.match_ [%expr Clang__bindings.ext_attr_get_kind cursor]
        (List.rev cases) in
    [%stri
       [%%meta Metapp.filter.structure_item Metapp.filter [%stri
       let convert
           cursor expr_of_cxcursor of_type_loc convert_declaration_name =
         [%e pattern_matching]]]] in
  let chan = open_out (prefix ^ "attributes.ml") in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
    Stubgen_common.output_warning_ml chan tool_name;
    let fmt = Format.formatter_of_out_channel chan in
    Format.fprintf fmt "%a@." Pprintast.structure
      ([%str
          [%%metapackage "metapp"]
          [%%metadir "config/.clangml_config.objs/byte"]] @
        [type_decl; convert]));
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
