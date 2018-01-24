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
open Lexer
open Lexing
open Utils

let name = "prin"

let rec prin chan t =
  match t with
  | Nil             -> Ok t
  | T               -> Printf.fprintf chan "T"; Ok t
  | Any             -> Printf.fprintf chan "*"; Ok t
  | Number n        -> Printf.fprintf chan "%Ld" n; Ok t
  | String s        -> Printf.fprintf chan "%s" s; Ok t
  | Function (s, _) -> Printf.fprintf chan "<%s>" s; Ok t
  | Symbol s        -> Printf.fprintf chan "%s" s; Ok t
  | Cons (a, Nil)   -> Interpreter.eval a >>= prin chan;
  | Cons (a, b)     -> Interpreter.eval a >>= prin chan |> ignore; prin chan b

let run t =
  !Interpreter.out_channel |> fun (_, chan) -> prin chan t

let hook = (name, run)
