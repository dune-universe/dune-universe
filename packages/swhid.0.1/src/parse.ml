(** [parse buf] parses a swhid from the [buf] [Stdlib.Lexing.lexbuf]. *)
let parse buf =
  try Ok (Menhir_parser.identifier Lexer.token buf) with
  | Lang.Parser_error s -> Error (Format.sprintf "parser error: %s" s)
  | Menhir_parser.Error -> Error (Format.sprintf "parser error: syntax error")
  | Lexer.Error s -> Error (Format.sprintf "lexer error: %s" s)

(** [from_string s] parses a swhid from string [s]. *)
let from_string s = parse (Lexing.from_string s)

(** [from_channel c] parses a swhid from channel [c] *)
let from_channel c = parse (Lexing.from_channel c)

(** [from_file f] parses a swhid from the file of name [f]. *)
let from_file f =
  let chan = open_in f in
  let result = parse (Lexing.from_channel chan) in
  close_in chan;
  result
