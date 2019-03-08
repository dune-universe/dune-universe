open Ppxlib

(** Structure generator *)
val from_str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t

(** Signature generator *)
val from_sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t

(** Return the name of the factory function derived from a type with the given name. *)
val _name_from_type_name : string -> string

(** Return the name of the factory derived from a type with a given [type_name] and for the
    constructor with [constructor_name].
*)
val _name_from_type_and_constructor_name : type_name: string -> constructor_name: string -> string
