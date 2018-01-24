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

let name = "catch"

let rec process e = function
  | Nil ->
    raise (Interpreter.Throw e)
  | Cons (Cons (v, hnd), others) ->
    if Equ.compare (e, v) = Ok T then
      Interpreter.eval hnd
    else
      process e others
  | _ -> Ok Nil

let rec run = function
  | Cons (Cons _ as prg, exns) ->
    begin
      let tl = Trace.get () in
      try Interpreter.eval prg
      with Interpreter.Throw e ->
        Trace.set tl;
        process e exns
    end
  | t -> Error.undefined t

let hook = (name, run)
