(** The to_string function should be named foo_to_string for variant foo, and
 *  just to_string for the special t variant
 *)
val to_string_function_name : enum_name: string -> string

(** The from_string function should be named foo_from_string for variant foo, and
 *  just from_string for the special t variant
 *)
val from_string_function_name : enum_name: string -> string

(** The from_string_exn function should be named foo_from_string_exn for variant foo, and
 *  just from_string_exn for the special t variant
 *)
val from_string_exn_function_name : enum_name: string -> string

(** Test whether a constructor is a "bare" constructor - that is it is
 *  declared in the form
 *  | Name
 *  for some name
 *)
val constructor_is_bare : Parsetree.constructor_declaration -> bool

(** Test whether a list of constructors are all "bare" *)
val constructors_are_bare : Parsetree.constructor_declaration list -> bool
