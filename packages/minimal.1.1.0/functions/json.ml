(*
 * Copyright (c) 2018 Xavier R. Guérin <copyright@applepine.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Machine
open Grammar
open Utils

let name = "json"

let rec convert = function
  | `Assoc a    -> convert_assoc a
  | `Bool false -> Ok Nil
  | `Bool true  -> Ok T
  | `Float f    -> Ok (Number (Int64.of_float f))
  | `Int i      -> Ok (Number (Int64.of_int i))
  | `Intlit i   -> Ok (String i)
  | `List l     -> convert_list l
  | `Null       -> Ok Nil
  | `String s   -> Ok (String s)
  | `Tuple l    -> convert_list l
  | `Variant _  -> Ok Nil

and convert_assoc = function
  | [] -> Ok Nil
  | (key, hd) :: tl ->
    convert hd >>= fun hd ->
    convert_assoc tl >>= fun tl ->
    Ok (Cons (Cons (String key, hd), tl))

and convert_list = function
  | [] -> Ok Nil
  | hd :: tl ->
    convert hd >>= fun hd ->
    convert_list tl >>= fun tl ->
    Ok (Cons (hd, tl))

let parse_file fn =
  try Yojson.Safe.from_file fn |> convert
  with
  | Sys_error e -> Error e
  | Yojson.Json_error e -> Error e

let parse_string str =
  try Yojson.Safe.from_string str |> convert
  with
  | Sys_error e -> Error e
  | Yojson.Json_error e -> Error e

let run closure = function
  | Cons (mode, Cons (str, Nil)) ->
    Interpreter.eval closure mode >>= fun mode ->
    Interpreter.eval closure str >>= fun str ->
    begin match mode, str with
      | Symbol "file", String fn -> parse_file fn
      | Symbol "string", String fn -> parse_string fn
      | mode, str -> Error.undefined (Cons (mode, Cons (str, Nil)))
    end
  | t -> Error.undefined t

let hook = (name, run)
