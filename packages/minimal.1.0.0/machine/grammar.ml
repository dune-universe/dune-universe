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

include Grammar_types

let rec to_string = function
  | T               -> "T"
  | Nil             -> "NIL"
  | Any             -> "_"
  | Function (s, _) -> "<" ^ s ^ ">"
  | Symbol s        -> Printf.sprintf "%s" s
  | Number n        -> Printf.sprintf "%Ld" n
  | String s        -> Printf.sprintf "\"%s\"" s
  | Cons _ as l     -> "(" ^ (list_to_string l) ^ ")"

and list_to_string = function
  | Cons (a, Nil)           -> to_string a
  | Cons (a, (Cons _ as b)) -> (to_string a) ^ " " ^ (list_to_string b)
  | Cons (a, b)             -> (to_string a) ^ " . " ^ (to_string b)
  | t                       -> to_string t

let print hdr t =
  Printf.printf "%s %s\n" hdr (to_string t)
