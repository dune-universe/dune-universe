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

let name = "join"

let rec join sep result = function
  | Nil -> Ok (String result)
  | String str ->
    if String.length result = 0 then
      Ok (String str)
    else
      Ok (String (result ^ sep ^ str))
  | Cons (String str, rest) ->
    if String.length result = 0 then
      join sep str rest
    else
      join sep (result ^ sep ^ str) rest
  | t -> Error.undefined t

let run = function
  | Cons (sep, Cons (lst, Nil))
  | Cons (sep, lst) ->
    Interpreter.eval sep >>= fun sep ->
    Interpreter.eval lst >>= fun lst ->
    begin match sep with
      | Nil -> join "" "" lst
      | String sep -> join sep "" lst
      | t -> Error.undefined t
    end
  | t -> Error.undefined t

let hook = (name, run)
