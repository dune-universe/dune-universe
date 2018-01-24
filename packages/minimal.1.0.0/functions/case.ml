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

let name = "case"

let rec proc m0 = function
  | Cons (Cons (m1, prg), _) when Equ.compare (m0, m1) = Ok T ->
    World.shift m0 |> ignore;
    Interpreter.eval prg
  | Cons (Cons (Any, prg), _) ->
    Interpreter.eval prg
  | Cons (_, rest) ->
    proc m0 rest
  | _ -> Ok Nil

let run = function
  | Cons (m0, clauses) ->
    Interpreter.eval m0 >>= fun m0 -> proc m0 clauses
  | t -> Error.undefined t

let hook = (name, run)
