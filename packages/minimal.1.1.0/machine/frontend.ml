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

open Grammar
open Lexer
open Sedlexing
open Utils

let parse_with_error lexbuf =
  try Syntax.parse Lexer.read Parser.entry lexbuf with
  | Syntax.ParseError e ->
    Printf.printf "%s\n" (Syntax.string_of_ParseError e);
    None

let eval l =
  Interpreter.eval Closure.empty l >>= World.shift

let parse lexbuf =
  match parse_with_error lexbuf with
  | Some value -> value
  | None -> raise (Failure "No input")

let repl () =
  let live = ref true
  in
  while !live do
    try
      Printf.printf ": ";
      read_line () |> Utf8.from_string |> Syntax.create_lexbuf |> parse |> function
      | Nil -> live := false
      | t -> match eval t with
        | Ok t -> Grammar.to_string t |> Printf.printf "-> %s\n"
        | Error err -> Printf.printf "%s\n" err
    with
    | Interpreter.Throw e ->
      Printf.printf "%s -- Uncaught exception\n" (Grammar.to_string e);
      Trace.reset ()
    | Failure _ -> ()
    | End_of_file -> live := false; ()
  done

let load_all buf =
  let res = ref Nil
  and live = ref true
  in
    while !live do
      try
        match parse buf |> eval with
        | Ok t -> res := t
        | Error err -> Printf.printf "%s\n" err; live := false
      with
      | Interpreter.Throw e ->
        Printf.printf "%s -- Uncaught exception\n" (Grammar.to_string e);
        flush stdout;
        Trace.reset ();
        res := e;
        live := false
      | Failure _
      | End_of_file -> live := false
      | e ->
        Printf.printf ">> %s\n" (Printexc.to_string e);
        flush stdout;
        Trace.reset ();
        res := Nil
    done;
    !res

let load filename =
  try
    let dsc = Unix.openfile filename [ Unix.O_RDONLY ] 0o640 in
    let inx = Unix.in_channel_of_descr dsc in
    let slx = Utf8.from_channel inx in
    let buf = Syntax.create_lexbuf ~file:filename slx in
    let res = load_all buf in
    Unix.close dsc;
    Ok res
  with _ ->
    Ok Nil
