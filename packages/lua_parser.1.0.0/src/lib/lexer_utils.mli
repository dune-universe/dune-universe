(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

val pspos : Lexing.lexbuf -> string
(** Prints out the current position for error handling. *)

val lexeme : Lexing.lexbuf -> string
(** Gets next token for the lexers buffer. *)

val new_line : Lexing.lexbuf -> unit
(** Informs the lexer of a new line. *)

val in_lcomm : Lexing.lexbuf -> unit
(** Start lexing a long comment. *)

val out_lcomm :
  Lexing.lexbuf -> (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a) -> 'a
(** Finish lexing a long comment. *)

val in_lstr : Lexing.lexbuf -> unit
(** Start lexing a long string. *)

val out_lstr :
  Lexing.lexbuf -> (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a) -> 'a
(** Finish lexing a long string. *)

val add_buf : char -> unit
(** Add a character to the lexers buffer. *)

val get_buf : unit -> string
(** Get the contents of the lexers buffer. *)

val nl_buf : unit -> unit
(** Add a newline to the lexers buffer. *)
