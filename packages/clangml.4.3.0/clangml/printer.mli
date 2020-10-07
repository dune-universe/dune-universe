val qual_type : Format.formatter -> Clang__ast.qual_type -> unit

val typed_value :
    (Format.formatter -> unit) -> Format.formatter -> Clang__ast.qual_type ->
      unit

val expr : Format.formatter -> Clang__ast.expr -> unit

val decl : Format.formatter -> Clang__ast.decl -> unit

val decls : Format.formatter -> Clang__ast.decl list -> unit

val stmt : Format.formatter -> Clang__ast.stmt -> unit

val translation_unit : Format.formatter -> Clang__ast.translation_unit -> unit
