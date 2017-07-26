open Ppx_core
open Ppx_type_conv.Std
open Type_conv

val str_gen : (structure, rec_flag * type_declaration list) Generator.t
val sig_gen : (signature, rec_flag * type_declaration list) Generator.t

val shape_extension : loc:Location.t -> core_type -> expression
val digest_extension : loc:Location.t -> core_type -> expression
