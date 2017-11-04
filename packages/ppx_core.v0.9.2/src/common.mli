open! Import

val lident : string -> Longident.t

val core_type_of_type_declaration : type_declaration -> core_type

val name_type_params_in_td : type_declaration -> type_declaration

val combinator_type_of_type_declaration
  :  type_declaration
  -> f:(loc:Location.t -> core_type -> core_type)
  -> core_type

val gen_symbol : ?prefix : string -> unit -> string
(** [gen_symbol ?prefix ()] generates a fresh variable name with [prefix].

    @param prefix default = "_x"
*)

val string_of_core_type : core_type -> string

val assert_no_attributes : attributes -> unit
val assert_no_attributes_in : Ast_traverse.iter

val get_type_param_name : (core_type * variance) -> string Loc.t
(** [get_tparam_id tp] @return the string identifier associated with [tp] if it is a type
    parameter. *)


(** Returns whether the given type declarations
    refer to themselves.

    [short_circuit] allows you to override the search for certain type expressions.

    [stop_on_functions] allows to disregard the recursive occurences appearing in arrow
    types. The default is to disregard them.
*)
val types_are_recursive
  :  ?stop_on_functions:bool
  -> ?short_circuit:(core_type -> bool option)
  -> type_declaration list
  -> bool

val really_recursive : rec_flag -> type_declaration list -> rec_flag

val loc_of_payload   : attribute -> Location.t
val loc_of_attribute : attribute -> Location.t

(** convert multi-arg function applications into a cascade of 1-arg applications *)
val curry_applications : expression -> expression

(** Encode a warning message into an 'ocaml.ppwarning' attribute which can be inserted in
    a generated Parsetree.  The compiler will be responsible for reporting the warning. *)
val attribute_of_warning : Location.t -> string -> attribute
