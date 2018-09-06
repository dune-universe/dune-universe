(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

(** Spel utilities. *)

open Spel_t

let mk_expr_full edesc locs text =
  { expr_desc = edesc;
    expr_loc = locs;
    expr_text = text }

let mk_expr_text edesc text =
  let locs =
    begin try (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()) with
    | _ ->
        (Lexing.dummy_pos, Lexing.dummy_pos)
    end
  in
  mk_expr_full edesc locs text

let mk_expr edesc =
  mk_expr_text edesc None

(* Cleanup *)

let is_not_empty e =
  begin match e.expr_desc with
  | E_lit (L_string "") -> false
  | _ -> true
  end

let rec spel_cleanup e =
  begin match e.expr_desc with
  | E_op (Op_concat, el) ->
      begin match List.filter is_not_empty el with
      | [] -> { expr_desc = E_lit (L_string "");
                expr_loc = e.expr_loc;
                expr_text = e.expr_text; }
      | [e] -> e
      | el -> { expr_desc = E_op (Op_concat, el);
                expr_loc = e.expr_loc;
                expr_text = e.expr_text; }
      end
  | _ -> e
  end

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
  let pos = { Lexing.
              pos_fname = file;
              pos_lnum = 1; (* Start lines at 1, not 0 *)
              pos_bol = 0;
              pos_cnum = 0; }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line ?(n=0) lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    { lcp with
      pos_lnum = lcp.pos_lnum + 1;
      pos_bol = lcp.pos_cnum; }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- { p with Lexing.pos_cnum = new_pos }

(** The last matched word. *)
let lexeme { stream } = Sedlexing.Utf8.lexeme stream

(** [ParseError (file, line, col, token)] *)
exception ParseError of (string * int * int * string)

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i, token %s"
    (file_to_string file)
    line cnum tok

let raise_ParseError lexbuf =
  let { pos } = lexbuf in
  let tok = lexeme lexbuf in
  let open Lexing in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Printf.fprintf stderr "Parse error: %s\n" (string_of_ParseError (pos.pos_fname, line, col, tok));
  raise @@ ParseError (pos.pos_fname, line, col, tok)

let sedlex_with_menhir lexer' parser' lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lexer' lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised parser'
  in
  begin try
    parser lexer
  with
  | Sedlexing.MalFormed
  | Sedlexing.InvalidCodepoint _
    -> raise_ParseError lexbuf
  end

let uparse_file parser ulexer f =
  let ic = open_in f in
  let buf = Sedlexing.Utf8.from_channel ic in
  let lexbuf = create_lexbuf ~file:f buf in
  begin try
    let results = sedlex_with_menhir ulexer parser lexbuf in
    close_in ic; results
  with
  | e -> close_in ic; raise e
  end

let uparse_string parser ulexer s =
  let buf = Sedlexing.Utf8.from_string s in
  let lexbuf = create_lexbuf ~file:"" buf in
  sedlex_with_menhir ulexer parser lexbuf

(* String buffers *)
let string_buff () = Buffer.create 256
let reset_string buff = Buffer.clear buff
let add_char_to_string buff c = Buffer.add_char buff c
let add_string_to_string buff s = Buffer.add_string buff s
let get_string buff = Buffer.contents buff

