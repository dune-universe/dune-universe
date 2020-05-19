val mkloc : 'a -> 'a Location.loc

val var_of_type_expr : Types.type_expr -> string option

val univar_of_type_expr : Types.type_expr -> string option

val core_type_of_type_expr : Types.type_expr -> Parsetree.core_type

val type_declaration :
    string -> Types.type_declaration -> Parsetree.type_declaration

val signature : Types.signature -> Parsetree.signature

val module_type : Types.module_type -> Parsetree.module_type
