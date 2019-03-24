(** Generating atd *)

open Atd
open Common

val of_vars : init:Ast.full_module -> input_vars -> Ast.full_module

val of_shape : init:Ast.full_module -> string -> result_type -> Ast.full_module

val parse_file : string -> Ast.full_module
