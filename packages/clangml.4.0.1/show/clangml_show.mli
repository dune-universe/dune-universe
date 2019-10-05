type 'a pp = Format.formatter -> 'a -> unit

type 'a show = 'a -> string

val pp_decl : Clang.Decl.t pp

val show_decl : Clang.Decl.t show

val pp_type : Clang.Type.t pp

val show_type : Clang.Type.t show

val pp_expr : Clang.Expr.t pp

val show_expr : Clang.Expr.t show

val pp_stmt : Clang.Stmt.t pp

val show_stmt : Clang.Stmt.t show

val pp_enum_constant : Clang.Enum_constant.t pp

val show_enum_constant : Clang.Enum_constant.t show

val pp_translation_unit : Clang.Translation_unit.t pp

val show_translation_unit : Clang.Translation_unit.t show

val pp_linkage_kind : Clang.Ast.linkage_kind pp

val show_linkage_kind : Clang.Ast.linkage_kind show

val pp_ext_stmtkind : Clang.clang_ext_stmtkind pp

val show_ext_stmtkind : Clang.clang_ext_stmtkind show

val pp_ext_typekind : Clang.clang_ext_typekind pp

val show_ext_typekind : Clang.clang_ext_typekind show

val pp_ext_declkind : Clang.clang_ext_declkind pp

val show_ext_declkind : Clang.clang_ext_declkind show
