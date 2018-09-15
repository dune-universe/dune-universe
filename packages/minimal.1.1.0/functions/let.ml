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

let name = "let"

let rec bind = function
  | Nil, fn -> Ok (Cons (fn, Nil))
  | Cons (Cons (args, values), rest), Function (_, body, closure) ->
    Interpreter.eval closure values >>=
    build rest name args body closure
  | t, _ -> Error.undefined t

and build rest name args body closure values =
  let closure = Interpreter.push closure args values in
  bind (rest, Function (Nil, body, closure))

let run closure = function
  | Cons (assignments, Cons (prg, Nil)) ->
    let fn = Function (Nil, prg, closure) in
    bind (assignments, fn) >>= Interpreter.eval closure
  | t -> Error.undefined t

let hook = (name, run)
