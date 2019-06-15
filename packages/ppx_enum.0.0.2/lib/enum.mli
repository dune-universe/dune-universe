open Ppxlib

(** Structure generator *)
val from_str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t

(** Signature generator *)
val from_sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t
