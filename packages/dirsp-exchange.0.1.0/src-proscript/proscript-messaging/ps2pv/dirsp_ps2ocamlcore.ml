(* Copyright 2021 Diskuv, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)
(** Parse ProScript source code into OCaml. *)

include Ast2ocaml

(* ------------ BEGIN: DEBUGGING UTILITY SECTION ---------------------- *)

let is_Statement_of test = function
  | `Statement st, _loc -> test st
  | _ -> false


let is_FunctionDeclaration_of test = function
  | `FunctionDeclaration st, _loc -> test st
  | _ -> false


let is_Const_with_identifier id = function
  | `Const l, _loc ->
      List.exists
        (function
          | an_id, _expression -> an_id = id )
        l
  | _ -> false


let is_Property_with_identifier id = function
  | `Property (an_id, _expression), _loc -> an_id = id
  | _ -> false


(* ------------ END:   DEBUGGING UTILITY SECTION ---------------------- *)

type parsing_options = { not_needed_yet : unit }

let init_parsing_options () = { not_needed_yet = () }

let read_all fn =
  let ch = open_in_bin fn in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  s


exception Invalid_file of string

let parse proscript_filename _p_opts =
  let open_patched_file fn = read_all fn in
  let read_buf contents = Ulexing.from_utf8_string contents in
  let parse inbuf =
    Globals.ifile := Filename.basename proscript_filename ;
    try Globals.menhir_with_ulex Lexer.main Parser.main inbuf with
    | Globals.LexingError (loc, msg) ->
        raise
          (Invalid_file
             (Printf.sprintf
                "%s at %s\n%!"
                msg
                (Lexerror.format_position (Globals.lexpos_of_loc loc)) ) )
    | Parser.Error ->
        raise
          (Invalid_file
             (Printf.sprintf
                "Unexpected token <%s> at %s\n%!"
                !Globals.lasttok
                (Lexerror.format_position
                   (Globals.lexpos_of_loc (Globals.getloc ())) ) ) )
    | Utf8.MalFormed ->
        raise
          (Invalid_file
             (Printf.sprintf
                "Invalid UTF-8 input character at %s\n%!"
                (Lexerror.format_position
                   (Globals.lexpos_of_loc (Globals.getloc ())) ) ) )
  in
  proscript_filename |> open_patched_file |> read_buf |> parse


let parse_and_translate
    proscript_filename
    output_file
    header
    types_module
    interface_module
    (p_opts : parsing_options)
    (t_ops : Ast2ocaml.translation_options) =
  let ast = parse proscript_filename p_opts in
  let print_translations () =
    try
      let ocaml_source_code =
        Ast2ocaml.translate
          ~wrapped:{ types_module; interface_module }
          ast
          ~t_ops
      in
      let channel = open_out output_file in
      output_string channel header ;
      output_string channel ocaml_source_code ;
      close_out channel
    with
    | Globals.LexingError (loc, msg) ->
        raise
          (Invalid_file
             (Printf.sprintf
                "%s at %s\n%!"
                msg
                (Lexerror.format_position (Globals.lexpos_of_loc loc)) ) )
    | Parser.Error ->
        raise
          (Invalid_file
             (Printf.sprintf
                "Unexpected token <%s> at %s\n%!"
                !Globals.lasttok
                (Lexerror.format_position
                   (Globals.lexpos_of_loc (Globals.getloc ())) ) ) )
    | Utf8.MalFormed ->
        raise
          (Invalid_file
             (Printf.sprintf
                "Invalid UTF-8 input character at %s\n%!"
                (Lexerror.format_position
                   (Globals.lexpos_of_loc (Globals.getloc ())) ) ) )
  in
  print_translations ()
