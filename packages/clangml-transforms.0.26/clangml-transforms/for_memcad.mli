val bin_op :
    Clang.Type.t -> Clang.Ast.expr -> Clang.Ast.binary_operator_kind ->
      Clang.Ast.expr -> Clang.Ast.expr_desc Clang.Ast.node

val int : Clang.Type.t

val integer_literal : ?location:Clang.Ast.source_location -> int -> Clang.Ast.expr

val stmts_of_stmt : Clang.Ast.stmt -> Clang.Ast.stmt list

val stmt_of_stmts : ?location:Clang.Ast.source_location ->
  Clang.Ast.stmt list -> Clang.Ast.stmt

val transform_decl : Clang.Decl.t -> Clang.Decl.t
