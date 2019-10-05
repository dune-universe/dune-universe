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

let prec_of_unary_operator (kind : Clang.Ast.unary_operator_kind) =
  match kind with
  | PostInc | PostDec -> 1, Postfix
  | PreInc | PreDec -> 1, Prefix
  | AddrOf
  | Deref
  | Plus | Minus
  | Not | LNot -> 2, Prefix
  | _ -> invalid_arg "prec_of_unary_operator"

let prec_of_binary_operator (kind : Clang.Ast.binary_operator_kind) =
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

let rec decl fmt (d : Clang.Decl.t) =
  match d.desc with
  | Function { linkage; function_type; name; body; _ } ->
      print_linkage fmt linkage;
      print_function_type fmt function_type name;
      print_function_body fmt body
  | Var { linkage; var_type = ty; var_name; var_init; _ } ->
      Format.fprintf fmt "@[%a%a%a;@]"
	print_linkage linkage
	(typed_value (fun fmt -> Format.pp_print_string fmt var_name)) ty
	print_variable_init var_init
  | RecordDecl { keyword; name; fields; _ } ->
      Format.fprintf fmt "@[%s@ %s@ {%a}@]"
        (Clang.ext_elaborated_type_get_keyword_spelling keyword)
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
      Format.fprintf fmt "%s:" (Clang.string_of_cxx_access_specifier specifier)
  | Constructor { class_name; parameters; initializer_list; body; explicit; defaulted; deleted; _ } ->
      Format.fprintf fmt "%t%s(%a)%t%t%a"
        (fun fmt ->
          if explicit then Format.fprintf fmt "explicit@ ")
        class_name
        print_parameters parameters
        (fun fmt ->
          match initializer_list with
          | [] -> ()
          | _ ->
              Format.fprintf fmt "@ :@ ";
              Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
                (fun fmt (name, value) ->
                  Format.fprintf fmt "%s(%a)" name expr value)
                fmt initializer_list)
        (fun fmt ->
          if defaulted then
            Format.fprintf fmt "@ =@ default";
          if deleted then
            Format.fprintf fmt "@ =@ delete")
        print_function_body body
  | Directive (Include { program_context; filename }) ->
      let pp_arg fmt =
        if program_context then
          Format.fprintf fmt "\"%s\"" filename
        else
          Format.fprintf fmt "<%s>" filename in
      Format.fprintf fmt "@\n#include %t@\n" pp_arg
  | LinkageSpec { language; decls = list } ->
      Format.fprintf fmt "@[extern@ \"%s\"@ {@ @[%a@]@ }@]"
        (Clang.extern_of_language language)
        decls list
  | _ -> failwith (Format.asprintf "Not implemented decl: %a" Clangml_show.pp_decl d)

and print_variable_init fmt init =
  match init with
  | None -> ()
  | Some value ->
      Format.fprintf fmt "@ =@ %a" expr value

and expr fmt e =
  expr_prec 15 fmt e

and expr_prec prec fmt (e : Clang.Expr.t) =
  match e.desc with
  | IntegerLiteral i ->
      Clang.Ast.pp_print_integer_literal fmt i
  | FloatingLiteral f ->
      Clang.Ast.pp_print_floating_literal fmt f
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
  | Call {
        callee = { desc = DeclRef { name = OperatorName kind }; _ };
        args = [lhs; rhs]} ->
      let kind = Clang.binary_of_overloaded_operator_kind kind in
      expr_prec prec fmt { e with desc = BinaryOperator { lhs; kind; rhs }}
  | Call { callee; args } ->
      maybe_parentheses 1 prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a(@[%a@])@]" (expr_prec 1) callee
	  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@ ")
	     (expr_prec 15)) args)
  | UnaryOperator { kind; operand } ->
      let op_prec, position = prec_of_unary_operator kind in
      begin match position with
      | Prefix ->
          maybe_parentheses op_prec prec fmt (fun fmt ->
	    Format.fprintf fmt "@[%s@ %a@]"
              (Clang.ext_unary_operator_get_opcode_spelling kind)
              (expr_prec op_prec) operand)
      | Postfix ->
          maybe_parentheses op_prec prec fmt (fun fmt ->
	    Format.fprintf fmt "@[%a@ %s@]" (expr_prec op_prec)
              operand (Clang.ext_unary_operator_get_opcode_spelling kind))
      end
  | BinaryOperator { lhs; kind; rhs } ->
      let op_prec, associativity = prec_of_binary_operator kind in
      let left_prec, right_prec =
        match associativity with
        | Left_to_right -> op_prec - 1, op_prec
        | Right_to_left -> op_prec, op_prec - 1 in
      maybe_parentheses op_prec prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a@ %s@ %a@]" (expr_prec left_prec) lhs
          (Clang.ext_binary_operator_get_opcode_spelling kind)
          (expr_prec right_prec) rhs)
  | DeclRef ident_ref ->
      print_ident_ref fmt ident_ref
  | Member { base = None; field = FieldName field } ->
      print_ident_ref fmt field.desc
  | Member { base = Some base; arrow; field } ->
      let arrow_str =
        if arrow then "->"
        else "." in
      let print_field fmt (field : Clang.Ast.field) =
        match field with
        | FieldName name -> print_ident_ref fmt name.desc
        | _ -> invalid_arg "print_field" in
      Format.fprintf fmt "@[%a%s%a@]" expr base arrow_str
        print_field field
  | Cast { kind = CStyle; qual_type = ty; operand } ->
      maybe_parentheses 2 prec fmt (fun fmt ->
        Format.fprintf fmt "@[(%a)@ %a@]" qual_type ty (expr_prec 4) operand)
  | New { qual_type = ty; init; _ } ->
      let format_init fmt (init : Clang.Expr.t option) =
        match init with
        | None -> ()
        | Some { desc = Construct { args; _ }; _ } ->
            Format.fprintf fmt "@[(%a)@]"
              (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
                expr) args
        | Some expr ->
            failwith
              (Format.asprintf "Unexpected constructor argument %a"
                 Clangml_show.pp_expr e) in
      Format.fprintf fmt "new@ %a%a" qual_type ty format_init init
  | Delete { argument; _ } ->
      Format.fprintf fmt "delete@ %a" expr argument
  | _ -> failwith (Format.asprintf "Not implemented expr %a" Clangml_show.pp_expr e)

and stmt fmt (s : Clang.Stmt.t) =
  match s.desc with
  | Null -> Format.pp_print_string fmt ";"
  | Compound list ->
      Format.fprintf fmt "@[{@[%a@]}@]" (Format.pp_print_list stmt) list
  | If { cond; then_branch; else_branch; _ } ->
      Format.fprintf fmt "@[if@ (@[%a@])@ %a%a@]"
	expr cond stmt then_branch print_else_branch else_branch
  | While { cond; body; _ } ->
      Format.fprintf fmt "@[while@ (@[%a@])@ %a@]"
	expr cond stmt body
  | Return None ->
      Format.fprintf fmt "@[return;@]"
  | Return (Some value) ->
      Format.fprintf fmt "@[return@ %a;@]" expr value
  | Decl d ->
      Format.pp_print_list decl fmt d
  | Expr e ->
      Format.fprintf fmt "@[%a;@]" expr e
  | _ -> failwith (Format.asprintf "Not implemented stmt: %a" Clangml_show.pp_stmt s)

and print_else_branch fmt else_branch =
  match else_branch with
  | None -> ()
  | Some else_branch ->
      Format.fprintf fmt "@[else@ %a@]" stmt else_branch

and print_linkage fmt linkage =
  match linkage with
  | Internal -> Format.fprintf fmt "static@ "
  | External
  | NoLinkage -> ()
  | _ ->
      failwith (Format.asprintf "Not implemented linkage: %a"
        Clangml_show.pp_linkage_kind linkage)

and print_parameters fmt parameters =
  let all_parameters = List.map Option.some parameters.non_variadic in
  let all_parameters =
    if parameters.variadic then all_parameters @ [None] else all_parameters in
  let print_parameter fmt (parameter : Clang.Ast.parameter option) =
    match parameter with
    | None -> Format.pp_print_string fmt "..."
    | Some { desc = { name; qual_type = ty; _ }} ->
        typed_value (fun fmt -> Format.pp_print_string fmt name) fmt ty in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@ ")
    print_parameter fmt all_parameters

and print_function_type fmt function_type name =
  typed_value
    (fun fmt -> Format.fprintf fmt "@[%a(%a)@]" print_declaration_name name
        (pp_print_option print_parameters) function_type.parameters)
    fmt function_type.result

and print_function_body fmt body =
  match body with
  | None -> Format.pp_print_string fmt ";"
  | Some body -> stmt fmt body

and print_ident_ref fmt ident_ref =
  let print_nested_name_specifier_component
      (component : Clang.Ast.nested_name_specifier_component) =
    match component with
    | Global -> Format.pp_print_string fmt "::"
    | NestedIdentifier s -> Format.fprintf fmt "%s::" s
    | NamespaceName name
    | NamespaceAliasName name ->
        Format.fprintf fmt "%s::" name
    | TypeSpec ty
    | TypeSpecWithTemplate ty ->
        Format.fprintf fmt "%a::" qual_type ty in
  Option.iter (List.iter print_nested_name_specifier_component)
    ident_ref.nested_name_specifier;
  print_declaration_name fmt ident_ref.name

and print_declaration_name fmt (name : Clang.Ast.declaration_name) =
  match name with
  | IdentifierName s
  | LiteralOperatorName s -> Format.pp_print_string fmt s
  | ConstructorName ty
  | ConversionFunctionName ty -> qual_type fmt ty
  | DestructorName ty -> Format.fprintf fmt "~%a" qual_type ty
  | OperatorName op ->
      Format.fprintf fmt "operator%s"
        (Clang.ext_overloaded_operator_get_spelling op)
  | _ -> failwith "Not implemented print_ident_ref.declaration_name"

and typed_value fmt_value fmt t =
  match t.desc with
  | Pointer t ->
      typed_value (fun fmt -> Format.fprintf fmt "@[*%t@]" fmt_value) fmt t
  | BuiltinType Void ->
      Format.fprintf fmt "@[void@ %t@]" fmt_value
  | BuiltinType Int ->
      Format.fprintf fmt "@[int@ %t@]" fmt_value
  | BuiltinType SChar ->
      Format.fprintf fmt "@[char@ %t@]" fmt_value
  | ConstantArray { element; size } ->
      typed_value (fun fmt -> Format.fprintf fmt "@[%t[%d]@]" fmt_value size) fmt element
  | Elaborated { keyword; named_type } ->
      Format.fprintf fmt "@[%s@ %a@]"
        (Clang.ext_elaborated_type_get_keyword_spelling keyword)
        (typed_value fmt_value) named_type
  | Record ident
  | Enum ident ->
      Format.fprintf fmt "@[%a@ %t@]" print_ident_ref ident fmt_value
  | _ ->
      failwith (
        Format.asprintf "Not implemented qual type: %a" Clangml_show.pp_type t)

and qual_type fmt t =
  typed_value (fun fmt -> ()) fmt t

and decls fmt decls =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space decl) decls

let translation_unit fmt (tu : Clang.Translation_unit.t) =
  decls fmt tu.desc.items
