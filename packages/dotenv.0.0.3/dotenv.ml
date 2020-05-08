(*----------------------------------------------------------------------------
  Copyright (c) 2019 José Nogueira

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software without
     specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)
open Base
module Sys = Caml.Sys
module Uchar = Caml.Uchar

let debug_log ~debug message = if debug then Stdio.print_endline @@ Printf.sprintf "Dotenv debug: %s" message

let space_and_control_chars = "[\\p{Zs}\\p{Cc}]*"

let letters_numbers_underscores = "[\\p{L}\\p{N}_]+"

let any_text = ".*"

let regexp =
  Printf.sprintf "^%s(%s)%s=(%s)$" space_and_control_chars letters_numbers_underscores space_and_control_chars any_text

let env_var_regex = Pcre.regexp ~flags:[ `UTF8 ] regexp

(*
  Lines function shamelessly copied from https://github.com/dbuenzli/uutf/blob/master/test/examples.ml
  and renamed to read_lines
 *)
let read_lines ?encoding src =
  let rec loop d buf acc =
    match Uutf.decode d with
    | `Uchar u ->
      ( match Uchar.to_int u with
      | 0x000A ->
        let line = Buffer.contents buf in
        Buffer.clear buf;
        loop d buf (line :: acc)
      | _ ->
        Uutf.Buffer.add_utf_8 buf u;
        loop d buf acc )
    | `End -> List.rev (Buffer.contents buf :: acc)
    | `Malformed _ ->
      Uutf.Buffer.add_utf_8 buf Uutf.u_rep;
      loop d buf acc
    | `Await -> assert false
  in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) []

let get_env_exports ?(path = ".env") ?encoding ~debug =
  debug_log ~debug @@ Printf.sprintf "Reading environment from path \"%s\"" path;
  match Sys.file_exists path with
  | true ->
    let env_file_string = Stdio.In_channel.read_all path in
    Some (read_lines ?encoding (`String env_file_string))
  | false ->
    debug_log ~debug @@ Printf.sprintf "\"%s\" file not found." path;
    None

let parse_read_lines ~debug ~f acc env_line =
  try
    match Pcre.extract ~rex:env_var_regex env_line |> Array.to_list with
    | _ :: name :: value :: _ ->
      let transform_rules =
        [ Caml.String.trim;
          String.substr_replace_all ~pattern:"'" ~with_:"\"" (*replace single quotes for double quotes *)
        ]
      in
      let name = String.uppercase name in
      let value = List.fold transform_rules ~init:value ~f:(fun acc f -> f acc) in
      f ~debug acc ~name ~value
    | _ -> failwith "Oh, well..."
  with _ ->
    debug_log ~debug @@ Printf.sprintf "Parse Error: \"%s\"" env_line;
    acc

let create_vars_list ~debug acc ~name ~value =
  debug_log ~debug @@ Printf.sprintf "Processing variable %s with value %s." name value;
  List.Assoc.add acc ~equal:String.equal name value

let add_to_env ~debug uunit ~name ~value =
  match Sys.getenv_opt name with
  | None ->
    debug_log ~debug @@ Printf.sprintf "Processing variable %s with value %s." name value;
    Unix.putenv name value
  | Some _ ->
    debug_log ~debug @@ Printf.sprintf "Environment variable %s already set. Skipping this one." name;
    uunit

let parse ?(debug = false) ?path ?encoding () =
  let parse_exports = parse_read_lines ~f:create_vars_list ~debug in
  match get_env_exports ?path ?encoding ~debug with
  | None -> []
  | Some exports -> List.fold ~init:[] ~f:parse_exports exports |> List.rev

let export ?(debug = false) ?path ?encoding () =
  let add_exports_to_env = parse_read_lines ~f:add_to_env ~debug () in
  match get_env_exports ?path ?encoding ~debug with
  | None -> ()
  | Some exports -> List.iter ~f:add_exports_to_env exports
