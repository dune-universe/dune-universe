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

let name = "while"

let rec proc cnd prg last =
  Interpreter.eval cnd >>= function
  | Nil -> Ok last
  | t ->
    let old = World.get "@" in
    World.set "@" t;
    Interpreter.eval prg >>= fun res ->
    World.set "@" old;
    proc cnd prg res

let run = function
  | Cons (a, Cons (b, Nil)) -> proc a b Nil
  | t -> Error.undefined t

let hook = (name, run)
