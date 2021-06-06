(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 SMTLIB parser} *)

(** {2 SMTLIB 2.6 API} *)
module V_2_6 : sig
  module Ast = Ast
  module Loc = Loc
  module Parser = Parser
  module Lexer = Lexer

  val parse_file_exn : string -> Ast.statement list
  (** Parse the given file.
      @raise Ast.Parse_error in case of error *)

  val parse_file : string -> (Ast.statement list, string) Result.result

  val parse_list : Lexing.lexbuf -> (Ast.statement list, string) Result.result
  (** @since 0.3 *)

  val parse_list_exn : Lexing.lexbuf -> Ast.statement list
  (** @since 0.3 *)

  val parse_chan_exn :
    ?filename:string ->
    in_channel ->
    Ast.statement list
  (** Parse the given channel.
      @raise Ast.Parse_error in case of error *)

  val parse_chan :
    ?filename:string ->
    in_channel ->
    (Ast.statement list, string) Result.result

  val parse_string_exn : string -> Ast.statement list
  (** Parse content of the string
      @raise Ast.Parse_error in case of error
      @since 0.2 *)

  val parse_string : string -> (Ast.statement list, string) Result.result
  (** Parse content of the string
      @since 0.2 *)
end
