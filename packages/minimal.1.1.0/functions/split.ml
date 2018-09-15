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

let name = "split"

let split = function
  | Nil, String str -> Ok (
      Array.init (String.length str) (fun i -> String.make 1 (String.get str i))
      |> Array.to_list
      |> ListLabels.fold_right ~f:(fun e acc -> Cons (String e, acc)) ~init:Nil
    )
  | String sep, String str ->
    let sep = Str.regexp sep in
    Ok (
      Str.split sep str
      |> ListLabels.fold_right ~f:(fun e acc -> Cons (String e, acc)) ~init:Nil
    )
  | a, b -> Error.undefined (Cons (a, b))

let run closure = function
  | Cons (sep, Cons (str, Nil))
  | Cons (sep, str) ->
    Interpreter.eval closure sep >>= fun sep ->
    Interpreter.eval closure str >>= fun str ->
    split (sep, str)
  | t -> Error.undefined t

let hook = (name, run)
