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
