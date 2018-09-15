(*
   Boilerplate for using sedlex with Menhir, based on
   https://github.com/Drup/llvm/blob/3c43000f4e86af5b9b368f50721604957d403750/test/Bindings/OCaml/kaleidoscope/src/syntax.ml
*)

(** The state of the parser, a stream and a position. *)
type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
       pos_lnum = lcp.pos_lnum + 1;
       pos_bol = lcp.pos_cnum;
    }

let set_line lexbuf n =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
       pos_lnum = n;
       pos_bol = lcp.pos_cnum;
    }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

(** [ParseError (file, line, col)] *)
exception ParseError of (string * int * int)

let raise_ParseError lexbuf =
  let { pos } = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  raise @@ ParseError (pos.pos_fname, line, col)

let string_of_ParseError (file, line, cnum) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, colunm %i"
    (file_to_string file)
    line cnum

let parse lex par lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lex lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised par
  in
  try
    parser lexer
  with _ -> raise_ParseError lexbuf
