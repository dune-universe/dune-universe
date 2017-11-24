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

open Lexer_util

(** Utilities for loading file *)

let string_of_file file =
  try
    let inchan = open_in_bin file in
    let len = in_channel_length inchan in
    let buf = Buffer.create len in
    Buffer.add_channel buf inchan len;
    close_in inchan;
    Buffer.contents buf
  with
    Sys_error err ->
      Printf.eprintf
        "Could not read the file %s, got error Sys_error %s\n@?"
        file
        err;
      raise (Sys_error err)

(** Utilities for CNL parsing *)

let parse parser lexer buf =
  try
    parser lexer buf
  with
  | LexError msg ->
      begin
        let pos = buf.Lexing.lex_start_p in
        let msg = Printf.sprintf "At line %d column %d: %s%!" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) msg in
        raise (LexError msg)
      end
  | _ ->
      begin
        let pos = buf.Lexing.lex_start_p in
        let msg = Printf.sprintf "At line %d column %d: syntax error%!" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) in
        raise (LexError msg)
      end

let parse_string p_fun s =
  let buf = Lexing.from_string s in
  p_fun buf

let parse_file p_fun f =
  let ic = open_in f in
  let buf = Lexing.from_channel ic in
  try
    let res = p_fun buf in
    close_in ic; res
  with
  | e ->
      close_in ic;
      Printf.fprintf stderr "Error in file %s%!\n" f; raise e

let parse_expr f : Cnl_t.cnl_expr = parse Cnl_parser.main_expr (Cnl_lexer.token (string_buff ())) f
let parse_cnl_expr_from_string s : Cnl_t.cnl_expr = parse_string parse_expr s

let parse_rule f : Cnl_t.cnl_rule = parse Cnl_parser.main (Cnl_lexer.token (string_buff ())) f
let parse_cnl_rule_from_string s : Cnl_t.cnl_rule = parse_string parse_rule s


let parse_bmd_spec f : Bmd_t.bmd_spec = parse Bmd_parser.main (Bmd_lexer.token (string_buff ())) f
let parse_bmd_spec_from_string s : Bmd_t.bmd_spec = parse_string parse_bmd_spec s
let parse_bmd_spec_from_file s : Bmd_t.bmd_spec = parse_file parse_bmd_spec s


