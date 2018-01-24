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

(** Spel lexer. *)

open Spel_util
open Spel_parser_j

let keyword_table =
  let tbl = Hashtbl.create 37 in
  begin
    List.iter (fun (key, data) -> Hashtbl.add tbl key data)
      [ "or", OR;
        "and", AND;
        "not", NOT;
        "true", TRUE;
        "false", FALSE;
        "null", NULL;
        "new", NEW;
        "anything_else", ANYTHING_ELSE;
        "context", CONTEXT;
        "conversation_start", CONVERSATION_START;
        "entities", ENTITIES;
        "input", INPUT;
        "intents", INTENTS;
        "output", OUTPUT;
      ]; tbl
  end

let uident = [%sedlex.regexp? Plus (id_continue|'-')]
let uintent = [%sedlex.regexp? Plus (id_continue|'-')]
let newline = [%sedlex.regexp? ('\010' | '\013' | "\013\010")]

let letter = [%sedlex.regexp? 'A'..'Z'|'a'..'z'|'_']
let ident_char = [%sedlex.regexp? 'A'..'Z'|'a'..'z'|'_'|'-'|'\''|'0'..'9']
let ident = [%sedlex.regexp? (letter, Star ident_char)]

let digit = [%sedlex.regexp? '0'..'9']
let frac = [%sedlex.regexp? '.', Star digit]
let exp = [%sedlex.regexp? ('e'|'E'), ('-'|'+'), Plus digit]
let integ = [%sedlex.regexp? Star digit]
let float = [%sedlex.regexp? Star digit, ((frac, Opt exp) | exp)]

let rec token sbuff lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | eof -> EOF ""
  | "==" -> EQUALEQUAL
  | "!=" -> NOTEQUAL
  | "<" -> LT
  | "<=" -> LTEQ
  | ">" -> GT
  | ">=" -> GTEQ
  | "&&" -> AND
  | "||" -> OR
  | "!" -> NOT
  | "." -> DOT
  | "?" -> QUESTION
  | ":" -> COLON
  | "," -> COMMA
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "{" -> LCURL
  | "}" -> RCURL
  | "[" -> LBRACKET
  | "]" -> RBRACKET
  | "+" -> PLUS
  | "-" -> MINUS
  | "*" -> MULT
  | "/" -> DIV
  | "%" -> MOD
  | ' ' | '\t' -> token sbuff lexbuf
  | newline -> (* Sedlexing.new_line lexbuf; *) token sbuff lexbuf
  | float -> REAL (float_of_string (Sedlexing.Utf8.lexeme buf))
  | integ -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | '"' -> reset_string sbuff; STRING (string sbuff lexbuf)
  | "'" -> reset_string sbuff; STRING (qstring sbuff lexbuf)
  | "#" -> intent sbuff lexbuf
  | "@" -> ENTITY (colon_ident sbuff lexbuf)
  | "$" -> VAR (colon_ident sbuff lexbuf)
  | ident ->
      let s = Sedlexing.Utf8.lexeme buf in
      begin try Hashtbl.find keyword_table (String.lowercase s)[@ocaml.warning "-3"]
      with Not_found -> IDENT s
      end
  | "?>" -> reset_string sbuff; CLOSEEXPR
  | any -> failwith "Unexpected character"
  | _ -> failwith "Unexpected character"
  end

and intent sbuff lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | uintent -> INTENT (Sedlexing.Utf8.lexeme buf)
  | _ -> failwith "Unexpected character after"
  end

and colon_ident sbuff lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | uident -> colon_ident_rest sbuff (Sedlexing.Utf8.lexeme buf) lexbuf
  | _ -> failwith "Unexpected character after"
  end

and colon_ident_rest sbuff colon_ident_name lexbuf =
  let buf = lexbuf.stream in
  begin match %sedlex buf with
  | ':', uident ->
      let token = Sedlexing.Utf8.lexeme buf in
      let ident = String.sub token 1 ((String.length token)-1) in
      (colon_ident_name, Some ident)
  | ':', '(' ->
      reset_string sbuff;
      (colon_ident_name, Some (qparen sbuff lexbuf))
  | _ -> (colon_ident_name,None)
  end

and string sbuff lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | '"' -> get_string sbuff
  | any -> add_string_to_string sbuff (Sedlexing.Utf8.lexeme buf); string sbuff lexbuf
  | _ -> failwith "Unexpected character"
  end

and qstring sbuff lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | "'" -> get_string sbuff
  | any -> add_string_to_string sbuff (Sedlexing.Utf8.lexeme buf); qstring sbuff lexbuf
  | _ -> failwith "Unexpected character"
  end

and qparen sbuff lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | ")" -> get_string sbuff
  | any -> add_string_to_string sbuff (Sedlexing.Utf8.lexeme buf); qparen sbuff lexbuf
  | _ -> failwith "Unexpected character"
  end

and body sbuff lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | "\\$" -> add_string_to_string sbuff "\\$"; body sbuff lexbuf
  | "$" -> let s = get_string sbuff in reset_string sbuff; body_variable sbuff s lexbuf
  | eof -> let s = get_string sbuff in EOF s (* End of string *)
  | "<?" -> let s = get_string sbuff in reset_string sbuff; OPENEXPR s  (* End of string *)
  | any -> add_string_to_string sbuff (Sedlexing.Utf8.lexeme buf); body sbuff lexbuf
  | _ -> failwith "Unexpected character"
  end

and body_variable sbuff s lexbuf =
  let buf = lexbuf.stream in
  begin match%sedlex buf with
  | uident -> BODYVAR (s,colon_ident_rest sbuff (Sedlexing.Utf8.lexeme buf) lexbuf)
  | _ -> add_string_to_string sbuff "$"; body sbuff lexbuf
  end

