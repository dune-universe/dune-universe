(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

val parse_from_chan : in_channel -> Ast.ast
(** Parse from a channel (i.e. stdin). *)

val parse_from_string : string -> Ast.ast
(** Parse from a string. *)
