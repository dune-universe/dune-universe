(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

val pp_lua : Ast.ast -> unit
(** Pretty print AST to stdout. *)

val pp_lua_str : Ast.ast -> string
(** Pretty print AST to a string. *)
