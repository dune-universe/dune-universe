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

let rec (==) a b =
  match a, b with
  (* Atoms. *)
  | T, T     -> true
  | Nil, Nil -> true
  | Any, _   -> true
  | _, Any   -> true
  | Number i, Number j -> i = j
  | String i, String j
  | Symbol i, Symbol j -> String.equal i j
  (* Functions. *)
  | Function (a0, b0, _), Cons (a1, Cons (b1, _))
  | Function (a0, b0, _), Function (a1, b1, _) -> a0 == a1 && b0 == b1
  (* Lists. *)
  | Cons (a0, b0), Cons (a1, b1) -> a0 == a1 && b0 == b1
  | _ -> false

let compare (a, b) =
  Ok (if a == b then T else Nil)

let run closure = function
  | Cons (a, Nil) ->
    Interpreter.eval closure a >>= fun a ->
    let body = Cons (Symbol "=", Cons (a, Cons (Symbol "a", Nil)))
    and args = Cons (Symbol "a", Nil)
    in
    Cons (Symbol "λ", Cons (args, Cons (body, Nil)))
    |> Interpreter.eval closure
  | Cons (a, Cons (b, Nil)) ->
    Interpreter.eval closure a >>= fun a ->
    Interpreter.eval closure b >>= fun b ->
    compare (a, b)
  | t -> Error.undefined t

let hook = (name, run)
