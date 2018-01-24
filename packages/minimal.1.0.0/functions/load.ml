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

let name = "load"

let regex = Str.regexp "^@lib/.*$"

let load = function
  | String fn
  | Cons (String fn, Nil) when Str.string_match regex fn 0 ->
    let prefix = Interpreter.get_prefix ()
    and subp = String.sub fn 5 ((String.length fn) - 5)
    in
    [ prefix ^ "/lib/" ^ subp; prefix ^ "/lib/minimal/" ^ subp ]
    |> ListLabels.find_opt ~f:Sys.file_exists
    |> begin function
      | Some p -> Frontend.load p
      | None -> Error.not_found subp
    end
  | String fn
  | Cons (String fn, Nil) ->
    if Sys.file_exists fn
    then Frontend.load fn
    else Error.not_found fn
  | t -> Error.undefined t

let run = function
  | Cons (a, Nil)
  | a -> Interpreter.eval a >>= fun a -> load a

let hook = (name, run)
