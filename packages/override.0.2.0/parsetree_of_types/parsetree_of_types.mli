module OCaml_version = Migrate_parsetree.OCaml_407

val mkloc : 'a -> 'a Location.loc

val var_of_type_expr : Types.type_expr -> string option

val univar_of_type_expr : Types.type_expr -> string option

val core_type_of_type_expr :
    Types.type_expr -> OCaml_version.Ast.Parsetree.core_type

val type_declaration :
    string -> Types.type_declaration ->
      OCaml_version.Ast.Parsetree.type_declaration

val signature : Types.signature -> OCaml_version.Ast.Parsetree.signature

val module_type : Types.module_type -> OCaml_version.Ast.Parsetree.module_type
