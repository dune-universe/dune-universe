open Migrate_parsetree.Ast_404


val make_str
   : string
  -> string Location.loc

val uncapitalize_str
   : string Location.loc
  -> string Location.loc


val make_ident
   : ?modname:string
  -> string
  -> Longident.t

val make_ident_loc
   : Location.t
  -> ?modname:string
  -> string
  -> Longident.t Location.loc


val make_exp_apply
   : Location.t
  -> Longident.t
  -> Parsetree.expression list
  -> Parsetree.expression

val make_exp_construct
   : Location.t
  -> Longident.t
  -> Parsetree.expression list
  -> Parsetree.expression

val make_exp_ident
   : Location.t
  -> ?modname:string
  -> string
  -> Parsetree.expression

val make_exp_fun
   : bool
  -> string
  -> Parsetree.expression
  -> Parsetree.expression

val make_exp_funs
   : bool
  -> string list
  -> Parsetree.expression
  -> Parsetree.expression

val make_exp_list
   : Parsetree.expression list
  -> Parsetree.expression


val make_pat_construct
   : Location.t
  -> Longident.t
  -> Parsetree.pattern list
  -> Parsetree.pattern

val make_pat_var
   : Location.t
  -> string
  -> Parsetree.pattern


val make_typ_arrow
   : Parsetree.core_type
  -> Parsetree.core_type
  -> Parsetree.core_type

val make_typ_arrows
   : Parsetree.core_type list
  -> Parsetree.core_type

val make_typ_constr
   : module_name:string
  -> type_name:string
  -> type_params:Parsetree.core_type list
  -> Parsetree.core_type

val make_typ_tuple
   : Parsetree.core_type list
  -> Parsetree.core_type

val make_typ_var
   : string
  -> Parsetree.core_type

val qualify_core_type
   : types:(string * string) list
  -> Parsetree.core_type
  -> Parsetree.core_type
