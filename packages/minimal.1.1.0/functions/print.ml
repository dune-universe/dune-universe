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

let name = "print"

let rec print closure chan t =
  match t with
  | Nil -> Ok t
  | Cons (a, Nil) ->
    Interpreter.eval closure a >>= fun r ->
    Printf.fprintf chan "%s" (Grammar.to_string r);
    Ok r
  | Cons (a, b) ->
    Interpreter.eval closure a >>= fun r ->
    Printf.fprintf chan "%s " (Grammar.to_string r);
    print closure chan b
  | _ ->
    Interpreter.eval closure t >>= fun r ->
    Printf.fprintf chan "%s" (Grammar.to_string r);
    Ok r

let run closure t =
  !Interpreter.out_channel |> fun (_, chan) -> print closure chan t

let hook = (name, run)
