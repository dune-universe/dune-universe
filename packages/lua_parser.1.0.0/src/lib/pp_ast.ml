(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Ast
open Sexp_pretty

let pp_ast_sexp ast =
  print_endline (sexp_to_string (sexp_of_ast ast))
(** Print AST as an S-expression. *)

let pp_ast_show ast = print_endline (show_ast ast)
(** Print AST in JSON *)
