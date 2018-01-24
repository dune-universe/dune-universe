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

let name = "="

let rec compare = function
  | T, T     -> Ok T
  | Nil, Nil -> Ok T
  | Any, _   -> Ok T
  | _, Any   -> Ok T
  | Number i, Number j when i = j            -> Ok T
  | String i, String j when String.equal i j -> Ok T
  | Symbol i, Symbol j when String.equal i j -> Ok T
  | Cons (a0, b0), Cons (a1, b1) when compare (a0, a1) = Ok T && compare (b0, b1) = Ok T -> Ok T
  | _ -> Ok Nil

let run = function
  | Cons (a, Cons (b, Nil)) ->
    Interpreter.eval a >>= fun a ->
    Interpreter.eval b >>= fun b ->
    compare (a, b)
  | t -> Error.undefined t

let hook = (name, run)
