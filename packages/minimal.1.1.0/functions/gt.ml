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

let name = ">"

let run closure = function
  | Cons (a, Nil) ->
    Interpreter.eval closure a >>=
    begin function
      | Number _ as n ->
        let body = Cons (Symbol ">", Cons (n, Cons (Symbol "a", Nil)))
        and args = Cons (Symbol "a", Nil)
        in
        Cons (Symbol "λ", Cons (args, Cons (body, Nil)))
        |> Interpreter.eval closure
      | a -> Error.undefined a
    end
  | Cons (a, Cons (b, Nil)) ->
    Interpreter.eval closure a >>= fun a ->
    Interpreter.eval closure b >>= fun b ->
    begin match a, b with
      | Number i, Number j when i > j -> Ok T
      | Number i, Number j -> Ok Nil
      | a, b -> Error.undefined (Cons (a, b))
    end
  | t -> Error.undefined t

let hook = (name, run)
