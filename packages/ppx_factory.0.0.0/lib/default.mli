open Ppxlib

(** Return the default expression for the given core type, located at [loc] or a located error
    if a default expression can't be deduced.
    E.g. [expr_from_core_type ~loc [%type int]] is [[%expr 0]].
*)
val expr_from_core_type : loc: Location.t -> core_type -> (expression, Loc_err.t) result

(** Same as [expr_from_core_type] but raises instead of returning a result *)
val expr_from_core_type_exn : loc: Location.t -> core_type -> expression

(** Structure generator *)
val from_str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t

(** Signature generator *)
val from_sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t

(** Return the name of the default value derived from a type with the given name. *)
val _name_from_type_name : string -> string
