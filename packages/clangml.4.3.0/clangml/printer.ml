let string_of_builtin_type (ty : Clang__ast.builtin_type) =
  match ty with
  | Void -> "void"
  | Bool -> "bool"
  | Char_S -> "char"
  | Float -> "float"
  | Int -> "int"
  | UInt -> "unsigned int"
  | _ ->
      Printf.sprintf "<unknown builtin type:%s>"
        (Clang__bindings.get_type_kind_spelling ty)

let maybe_parentheses in_prec out_prec fmt k =
  if in_prec >= out_prec then
    Format.fprintf fmt "(@[%t@])" k
  else
    k fmt

let pp_print_option pp_print_value fmt opt =
  match opt with
  | None -> ()
  | Some value -> pp_print_value fmt value

let c_escape_char fmt c =
  match c with
  | '"' | '\'' | '\\' ->  Format.fprintf fmt "\\%c" c
  | '\x07' -> Format.pp_print_string fmt "\\a"
  | '\x08' -> Format.pp_print_string fmt "\\b"
  | '\x09' -> Format.pp_print_string fmt "\\t"
  | '\x0A' -> Format.pp_print_string fmt "\\n"
  | '\x0B' -> Format.pp_print_string fmt "\\v"
  | '\x0C' -> Format.pp_print_string fmt "\\f"
  | '\x0D' -> Format.pp_print_string fmt "\\r"
  | '\x00' .. '\x20' -> Format.fprintf fmt "\\x%02X" (int_of_char c)
  | _ -> Format.pp_print_char fmt c

type associativity =
  | Left_to_right
  | Right_to_left

type unary_position =
  | Prefix
  | Postfix

let prec_of_unary_operator (kind : Clang__ast.unary_operator_kind) =
  match kind with
  | PostInc | PostDec -> 1, Postfix
  | PreInc | PreDec -> 1, Prefix
  | AddrOf
  | Deref
  | Plus | Minus
  | Not | LNot -> 2, Prefix
  | _ -> invalid_arg "prec_of_unary_operator"

let prec_of_binary_operator (kind : Clang__ast.binary_operator_kind) =
  match kind with
  | Mul | Div | Rem -> 3, Left_to_right
  | Add | Sub -> 4, Left_to_right
  | Shl | Shr -> 5, Left_to_right
  | LT | GT | LE | GE -> 6, Left_to_right
  | EQ | NE -> 7, Left_to_right
  | And -> 8, Left_to_right
  | Xor -> 9, Left_to_right
  | Or -> 10, Left_to_right
  | LAnd -> 11, Left_to_right
  | LOr -> 12, Left_to_right
  | Assign
  | MulAssign
  | DivAssign
  | RemAssign
  | AddAssign
  | SubAssign
  | ShlAssign
  | ShrAssign
  | AndAssign
  | XorAssign
  | OrAssign -> 14, Right_to_left
  | Comma -> 15, Left_to_right
  | _ -> invalid_arg "prec_of_binary_operator"

let pp_comma fmt () =
  Format.fprintf fmt ",@ "

let rec decl fmt (d : Clang__ast.decl) =
  match d.desc with
  | Function { linkage; function_type; name; body; _ } ->
      Format.fprintf fmt "@[<v>%a%a%a@]"
        pp_linkage linkage
        pp_function_type (function_type, name)
        pp_function_body body
  | Var var_decl ->
      Format.fprintf fmt "@[%a;@]" pp_var_decl var_decl
  | EnumDecl { name; constants; _ } ->
      let pp_constant fmt (constant : Clang__ast.enum_constant) =
        Format.pp_print_string fmt constant.desc.constant_name;
        constant.desc.constant_init |> Option.iter (fun init ->
          Format.fprintf fmt "@ =@ %a" expr init) in
      Format.fprintf fmt "@[<v>@[enum@ %s@ {@]@,%a};@]" name
        (Format.pp_print_list ~pp_sep:pp_comma pp_constant) constants
  | RecordDecl { keyword; name; fields; _ } ->
      Format.fprintf fmt "@[<v>@[%s@ %s@ {@]@,%a};@]"
        (Clang__bindings.ext_elaborated_type_get_keyword_spelling keyword)
        name
        (Format.pp_print_list decl)
        fields
  | Field { name; qual_type = ty; bitwidth; _ } ->
      Format.fprintf fmt "@[%a%t;@]"
        (typed_value (fun fmt -> Format.pp_print_string fmt name)) ty
        (fun fmt ->
          match bitwidth with
          | None -> ()
          | Some bitwidth -> Format.fprintf fmt "@ :@ %a" expr bitwidth)
  | AccessSpecifier specifier ->
      Format.fprintf fmt "%s:" (Clang__utils.string_of_cxx_access_specifier specifier)
  | Constructor { class_name; parameters; initializer_list; body; explicit; defaulted; deleted; _ } ->
      Format.fprintf fmt "%t%s(%a)%t%t%a"
        (fun fmt ->
          if explicit then Format.fprintf fmt "explicit@ ")
        class_name
        pp_parameters parameters
        (fun fmt ->
          match initializer_list with
          | [] -> ()
          | _ ->
              Format.fprintf fmt "@ :@ ";
              Format.pp_print_list ~pp_sep:pp_comma
                (fun fmt (name, value) ->
                  Format.fprintf fmt "%s(%a)" name expr value)
                fmt initializer_list)
        (fun fmt ->
          if defaulted then
            Format.fprintf fmt "@ =@ default";
          if deleted then
            Format.fprintf fmt "@ =@ delete")
        pp_function_body body
  | Directive (Include { program_context; filename }) ->
      let pp_arg fmt =
        if program_context then
          Format.fprintf fmt "\"%s\"" filename
        else
          Format.fprintf fmt "<%s>" filename in
      Format.fprintf fmt "@\n#include %t@\n" pp_arg
  | Directive (Ifdef var) ->
      Format.fprintf fmt "@\n#ifdef %s@\n" var
  | Directive (Ifndef var) ->
      Format.fprintf fmt "@\n#ifndef %s@\n" var
  | Directive Endif ->
      Format.fprintf fmt "@\n#endif@\n"
  | LinkageSpec { language; decls = list } ->
      Format.fprintf fmt "@[extern@ \"%s\"@ {@ @[%a@]@ }@]"
        (Clang__utils.extern_of_language language)
        decls list
  | _ ->
      Format.fprintf fmt {|@[\<not implemented decl: %a>@]|}
        (Refl.pp [%refl: Clang__ast.decl] []) d

and pp_var_decl fmt (var_decl : Clang__ast.var_decl_desc) =
  match var_decl with { linkage; var_type = ty; var_name; var_init; _ } ->
  Format.fprintf fmt "@[%a%a%a@]"
    pp_linkage linkage
    (typed_value (fun fmt -> Format.pp_print_string fmt var_name)) ty
    pp_variable_init var_init

and pp_variable_init fmt init =
  match init with
  | None -> ()
  | Some value ->
      Format.fprintf fmt "@ =@ %a" expr value

and expr fmt e =
  expr_prec 15 fmt e

and expr_prec prec fmt (e : Clang__ast.expr) =
  match e.desc with
  | IntegerLiteral i ->
      Clang__ast_utils.pp_print_integer_literal fmt i
  | FloatingLiteral f ->
      Clang__ast_utils.pp_print_floating_literal fmt f
  | CharacterLiteral { kind; value } ->
      begin
        match kind with
        | Ascii -> Format.fprintf fmt "'%a'" c_escape_char (char_of_int value)
        | _ -> failwith "Not implemented character kind"
      end
  | StringLiteral { bytes } ->
      Format.pp_print_string fmt "\"";
      String.iter (c_escape_char fmt) bytes;
      Format.pp_print_string fmt "\""
  | BoolLiteral b ->
      Format.pp_print_bool fmt b
  | ArraySubscript { base; index } ->
      maybe_parentheses 1 prec fmt (fun fmt ->
        Format.fprintf fmt "@[%a[@[%a@]]@]" (expr_prec 1) base (expr_prec 15)
          index)
  | Call {
        callee = { desc = DeclRef { name = OperatorName kind }; _ };
        args = [lhs; rhs]} ->
      let kind = Clang__utils.binary_of_overloaded_operator_kind kind in
      expr_prec prec fmt { e with desc = BinaryOperator { lhs; kind; rhs }}
  | Call { callee; args } ->
      maybe_parentheses 1 prec fmt (fun fmt ->
        Format.fprintf fmt "@[%a(@[%a@])@]" (expr_prec 1) callee
          (Format.pp_print_list ~pp_sep:pp_comma
             (expr_prec 15)) args)
  | UnaryOperator { kind; operand } ->
      let op_prec, position = prec_of_unary_operator kind in
      begin match position with
      | Prefix ->
          maybe_parentheses op_prec prec fmt (fun fmt ->
            Format.fprintf fmt "@[%s@ %a@]"
              (Clang__bindings.ext_unary_operator_get_opcode_spelling kind)
              (expr_prec op_prec) operand)
      | Postfix ->
          maybe_parentheses op_prec prec fmt (fun fmt ->
            Format.fprintf fmt "@[%a@ %s@]" (expr_prec op_prec)
              operand (Clang__bindings.ext_unary_operator_get_opcode_spelling kind))
      end
  | BinaryOperator { lhs; kind; rhs } ->
      let op_prec, associativity = prec_of_binary_operator kind in
      let left_prec, right_prec =
        match associativity with
        | Left_to_right -> op_prec - 1, op_prec
        | Right_to_left -> op_prec, op_prec - 1 in
      maybe_parentheses op_prec prec fmt (fun fmt ->
        Format.fprintf fmt "@[%a@ %s@ %a@]" (expr_prec left_prec) lhs
          (Clang__bindings.ext_binary_operator_get_opcode_spelling kind)
          (expr_prec right_prec) rhs)
  | DeclRef ident_ref ->
      pp_ident_ref fmt ident_ref
  | Member { base = None; field = FieldName field } ->
      pp_ident_ref fmt field.desc
  | Member { base = Some base; arrow; field } ->
      let arrow_str =
        if arrow then "->"
        else "." in
      let pp_field fmt (field : Clang__ast.field) =
        match field with
        | FieldName name -> pp_ident_ref fmt name.desc
        | _ -> invalid_arg "print_field" in
      Format.fprintf fmt "@[%a%s%a@]" expr base arrow_str
        pp_field field
  | Cast { kind = CStyle; qual_type = ty; operand } ->
      maybe_parentheses 2 prec fmt (fun fmt ->
        Format.fprintf fmt "@[(%a)@ %a@]" pp_qual_type ty (expr_prec 4) operand)
  | New { qual_type = ty; init; _ } ->
      let format_init fmt (init : Clang__ast.expr option) =
        match init with
        | None -> ()
        | Some { desc = Construct { args; _ }; _ } ->
            Format.fprintf fmt "@[(%a)@]"
              (Format.pp_print_list ~pp_sep:pp_comma expr) args
        | Some expr ->
            failwith
              (Format.asprintf "Unexpected constructor argument %a"
                 (Refl.pp [%refl: Clang__ast.expr] []) e) in
      Format.fprintf fmt "new@ %a%a" pp_qual_type ty format_init init
  | Delete { argument; _ } ->
      Format.fprintf fmt "delete@ %a" expr argument
  | ConditionalOperator { cond; then_branch; else_branch } ->
      begin match then_branch with
      | Some then_branch ->
          Format.fprintf fmt "%a ? %a : %a" expr cond expr then_branch expr
            else_branch
      | None ->
          Format.fprintf fmt "%a ?: %a" expr cond expr else_branch
      end
  | _ ->
      Format.fprintf fmt "<not implemented expr: %a>"
        (Refl.pp [%refl: Clang__ast.expr] []) e

and pp_condition_variable fmt
    ((condition_variable : Clang__ast.var_decl option), cond) =
  match condition_variable with
  | Some condition_variable -> pp_var_decl fmt condition_variable.desc
  | None -> expr fmt cond

and stmt fmt (s : Clang__ast.stmt) =
  match s.desc with
  | Null -> Format.pp_print_string fmt ";"
  | Break -> Format.pp_print_string fmt "break;"
  | Case { lhs; rhs; body } ->
      let pp_rhs fmt =
        match rhs with
        | None -> ()
        | Some rhs -> Format.fprintf fmt " .. %a" expr rhs in
      Format.fprintf fmt "@[case@ %a%t:@]@ %a" expr lhs pp_rhs stmt body
  | Default body ->
      Format.fprintf fmt "default:@ %a" stmt body
  | Switch { condition_variable; cond; body; _ } ->
      Format.fprintf fmt "@[switch@ (@[%a@])@ %a@]"
        pp_condition_variable (condition_variable, cond) stmt body
  | Compound list ->
      Format.fprintf fmt "@[{@[<v>%a@]}@]" (Format.pp_print_list stmt) list
  | If { condition_variable; cond; then_branch; else_branch; _ } ->
      Format.fprintf fmt "@[if@ (@[%a@])@ %a%a@]"
        pp_condition_variable (condition_variable, cond) stmt then_branch
        pp_else_branch else_branch
  | While { cond; body; _ } ->
      Format.fprintf fmt "@[while@ (@[%a@])@ %a@]"
        expr cond stmt body
  | For { init; condition_variable; cond; inc; body } ->
      Format.fprintf fmt "@[for@ (@[%t@];@ @[%t@];@ @[%t@])@ %a@]"
        (fun fmt -> init |> Option.iter @@ stmt_without_semicolon fmt)
        (fun fmt -> cond |> Option.iter @@ fun cond ->
          pp_condition_variable fmt (condition_variable, cond))
        (fun fmt -> inc |> Option.iter @@ stmt_without_semicolon fmt)
        stmt body
  | ForRange { var = { desc = { var_name; var_type; _ }}; range; body } ->
      Format.fprintf fmt "@[for@ (@[%a@ :@ %a@])@ %a@]"
        (typed_value (fun fmt -> Format.pp_print_string fmt var_name)) var_type
        expr range stmt body
  | Return None ->
      Format.fprintf fmt "@[return;@]"
  | Return (Some value) ->
      Format.fprintf fmt "@[return@ %a;@]" expr value
  | Decl d ->
      Format.pp_print_list decl fmt d
  | Expr e ->
      Format.fprintf fmt "@[%a;@]" expr e
  | _ ->
      Format.fprintf fmt "<not implemented stmt: %a>"
        (Refl.pp [%refl: Clang__ast.stmt] []) s

and stmt_without_semicolon fmt (s : Clang__ast.stmt) =
  match s.desc with
  | Decl [{desc = Var var_decl}] ->
      pp_var_decl fmt var_decl
  | Expr e ->
      expr fmt e
  | _ ->
      Format.fprintf fmt "<not implemented stmt_without_semicolon: %a>"
        (Refl.pp [%refl: Clang__ast.stmt] []) s

and pp_else_branch fmt else_branch =
  match else_branch with
  | None -> ()
  | Some else_branch ->
      Format.fprintf fmt "@[else@ %a@]" stmt else_branch

and pp_linkage fmt linkage =
  match linkage with
  | Internal -> Format.fprintf fmt "static@ "
  | External
  | NoLinkage -> ()
  | _ ->
      failwith (Format.asprintf "Not implemented linkage: %a"
        (Refl.pp [%refl: Clang__ast.linkage_kind] []) linkage)

and pp_parameters fmt parameters =
  let all_parameters = List.map Option.some parameters.non_variadic in
  let all_parameters =
    if parameters.variadic then all_parameters @ [None] else all_parameters in
  let pp_parameter fmt (parameter : Clang__ast.parameter option) =
    match parameter with
    | None -> Format.pp_print_string fmt "..."
    | Some { desc = { name; qual_type = ty; _ }} ->
        typed_value (fun fmt -> Format.pp_print_string fmt name) fmt ty in
  Format.pp_print_list
    ~pp_sep:pp_comma
    pp_parameter fmt all_parameters

and pp_function_type fmt (function_type, name) =
  typed_value
    (fun fmt -> Format.fprintf fmt "@[%a(@]@,@[%a)@]" pp_declaration_name name
        (pp_print_option pp_parameters) function_type.parameters)
    fmt function_type.result

and pp_function_body fmt body =
  match body with
  | None -> Format.pp_print_string fmt ";"
  | Some body -> Format.fprintf fmt "@ %a" stmt body

and pp_ident_ref fmt ident_ref =
  let pp_nested_name_specifier_component
      (component : Clang__ast.nested_name_specifier_component) =
    match component with
    | Global -> Format.pp_print_string fmt "::"
    | NestedIdentifier s -> Format.fprintf fmt "%s::" s
    | NamespaceName name
    | NamespaceAliasName name ->
        Format.fprintf fmt "%s::" name
    | TypeSpec ty
    | TypeSpecWithTemplate ty ->
        Format.fprintf fmt "%a::" pp_qual_type ty in
  Option.iter (List.iter pp_nested_name_specifier_component)
    ident_ref.nested_name_specifier;
  pp_declaration_name fmt ident_ref.name;
  if ident_ref.template_arguments <> [] then
    Format.fprintf fmt "<%a>"
      (Format.pp_print_list ~pp_sep:pp_comma pp_template_argument)
      ident_ref.template_arguments

and pp_template_argument fmt (argument : Clang__ast.template_argument) =
  match argument with
  | Type ty -> pp_qual_type fmt ty
  | ExprTemplateArgument e -> expr fmt e
  | _ ->
      Format.fprintf fmt "@[<not implemented template argument: %a>@]"
        (Refl.pp [%refl: Clang__ast.template_argument] []) argument

and pp_declaration_name fmt (name : Clang__ast.declaration_name) =
  match name with
  | IdentifierName s
  | LiteralOperatorName s -> Format.pp_print_string fmt s
  | ConstructorName ty
  | ConversionFunctionName ty -> pp_qual_type fmt ty
  | DestructorName ty -> Format.fprintf fmt "~%a" pp_qual_type ty
  | OperatorName op ->
      Format.fprintf fmt "operator%s"
        (Clang__bindings.ext_overloaded_operator_get_spelling op)
  | _ -> failwith "Not implemented pp_ident_ref.declaration_name"

and typed_value fmt_value fmt t =
  let fmt_value =
    if t.const then
      (fun fmt -> Format.fprintf fmt "@[const %t@]" fmt_value)
    else
      fmt_value in
  match t.desc with
  | Pointer t ->
      typed_value (fun fmt -> Format.fprintf fmt "@[*%t@]" fmt_value) fmt t
  | LValueReference t ->
      typed_value (fun fmt -> Format.fprintf fmt "@[&%t@]" fmt_value) fmt t
  | RValueReference t ->
      typed_value (fun fmt -> Format.fprintf fmt "@[&&%t@]" fmt_value) fmt t
  | BuiltinType ty ->
      Format.fprintf fmt "@[%s@ %t@]" (string_of_builtin_type ty) fmt_value
  | ConstantArray { element; size_as_expr = Some size_as_expr; _ } ->
      typed_value
        (fun fmt -> Format.fprintf fmt "@[%t[%a]@]" fmt_value expr size_as_expr)
        fmt element
  | ConstantArray { element; size_as_expr = None; size } ->
      typed_value
        (fun fmt -> Format.fprintf fmt "@[%t[%d]@]" fmt_value size) fmt
        element
  | Elaborated { keyword; named_type } ->
      Format.fprintf fmt "@[%s@ %a@]"
        (Clang__bindings.ext_elaborated_type_get_keyword_spelling keyword)
        (typed_value fmt_value) named_type
  | Record ident
  | Enum ident
  | Typedef ident ->
      Format.fprintf fmt "@[%a@ %t@]" pp_ident_ref ident fmt_value
  | Auto ->
      Format.fprintf fmt "@[auto@ %t@]" fmt_value
  | FunctionType { result; parameters; _ } ->
      typed_value (fun fmt -> Format.fprintf fmt "@[(%t)(%t)@]"
          fmt_value (fun fmt -> parameters |> Option.iter @@ pp_parameters fmt))
        fmt result
  | _ ->
      Format.fprintf fmt {|@[\<not implemented qual type: %a>@]|}
        (Refl.pp [%refl: Clang__ast.qual_type] []) t

and pp_qual_type fmt t =
  typed_value (fun fmt -> ()) fmt t

and decls fmt decls =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space decl) decls

let translation_unit fmt (tu : Clang__ast.translation_unit) =
  decls fmt tu.desc.items

let qual_type = pp_qual_type
