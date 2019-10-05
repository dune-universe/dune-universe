val qual_type : Format.formatter -> Clang.Type.t -> unit

val typed_value : (Format.formatter -> unit) -> Format.formatter -> Clang.Type.t -> unit

val expr : Format.formatter -> Clang.Expr.t -> unit

val decl : Format.formatter -> Clang.Decl.t -> unit

val stmt : Format.formatter -> Clang.Stmt.t -> unit

val translation_unit : Format.formatter -> Clang.Translation_unit.t -> unit
