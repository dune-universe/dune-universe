module Type = Wktxt_type
module Mapper = Wktxt_mapper

(** [ParsingError (line, column, lexeme)]  *)
exception ParsingError of int * int * string

(** [doc_from_lexbuf lex] parse [lex] and return the resulting {!type:Type.document}.
    Raise {!exception:ParsingError} in case of failure *)
let doc_from_lexbuf : Lexing.lexbuf -> Type.document = fun lexbuf ->
  try
    Wktxt_lexer.newline := true ;
    Wktxt_lexer.last_def_term_line := 0 ;
    Wktxt_lexer.last_def_term_depth := 0 ;
    Wktxt_lexer.in_table := false ;
    Wktxt_lexer.header_isopen := false ;
    Wktxt_parser.document Wktxt_lexer.main lexbuf
    |> Mapper.normalize
  with _ ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let lexeme = Lexing.lexeme lexbuf in
    raise (ParsingError (line, col, lexeme))

(** See {!val:doc_from_lexbuf} *)
let doc_from_string
  : string -> Type.document =
  fun string ->
  doc_from_lexbuf (Lexing.from_string string)

(** See {!val:doc_from_lexbuf} *)
let doc_from_channel
  : in_channel -> Type.document =
  fun chan ->
  doc_from_lexbuf (Lexing.from_channel chan)

(** See {!val:doc_from_lexbuf} *)
let doc_from_file
  : string -> Type.document =
  fun file ->
  let chan = open_in file in
  let doc = doc_from_channel chan in
  close_in chan ;
  doc

(** [output_document out doc]
    Run through the parsed tree [doc]
    and print its content translated into HTML code using the [out] function.
*)
let output_document : (string -> unit) -> Type.document -> unit =
  Wktxt_output.output_document

(** [doc_to_string doc]
    Return the HTML version of doc, as a string. *)
let doc_to_string
  : Type.document -> string =
  fun doc ->
  let buffer = Buffer.create 4096 in
  let () = output_document (Buffer.add_string buffer) doc in
  Buffer.contents buffer

(** [doc_to_chan chan doc]
    Print [doc] (as HTML) to [chan].
    See {!val:output_document} *)
let doc_to_chan
  : out_channel -> Type.document -> unit =
  fun chan doc ->
  Wktxt_output.output_document (output_string chan) doc

(** [doc_to_file filename doc]
    Create (or open and truncate) [filename] and write [doc] (as HTML) into it.
    See {!val:output_document} *)
let doc_to_file filename doc =
  let chan = open_out filename in
  let () = doc_to_chan chan doc in
  close_out chan
