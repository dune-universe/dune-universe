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

open Grammar
open Utils

(*
 * Exception.
 *)

exception Throw of Grammar.t

(*
 * Global references.
 *)

let in_channel = ref ("stdin", stdin, Syntax.create_lexbuf (Sedlexing.Utf8.from_channel stdin))
let out_channel = ref ("stdout", stdout)

(*
 * Prefix management.
 *)

let prefix = ref "."

let get_prefix () =
  !prefix

let set_prefix v =
  prefix := v

(*
 * Concatenation.
 *)

let rec conc next = function
  | Cons (a, (Cons _ as b)) -> Cons (a, conc next b)
  | Cons (a, b) -> Cons (a, next)
  | a -> next

(*
 * Evaluation function.
 *)

let rec push closure args values =
  match args, values with
  (*
   * Pattern matching.
   *)
  | Any, _ -> closure
  | Nil, Nil -> closure
  (*
   * Symbol binding.
   *)
  | Symbol s, v ->
    Closure.add s v closure
  (*
   * Recursive descent.
   *)
  | Cons (a0, a1), Cons (v0, v1) ->
    Closure.merge (push closure a0 v0) (push closure a1 v1)
  (*
   * Error scenarios.
   *)
  | _, _ -> closure

and curry closure = function
  | Function (Cons (a0, aN), b, c0), Cons (v0, vN) ->
    eval closure v0 >>= fun v0 ->
    curry closure (Function (aN, b, c0), vN) >>=
    begin function
      | Function (aN, _, cN) -> Ok (Function (aN, b, push cN a0 v0))
      | l -> Error.cannot_execute l
    end
  | fn, _ -> Ok fn

and exec closure values = function
  | Symbol _ as s -> resolve closure s >>= exec closure values
  | Internal (_, fn) -> fn closure values
  | Function _ as fn ->
    curry closure (fn, values) >>=
    begin function
      | Function (Nil, body, closure) -> eval closure body
      | res -> Ok res
    end
  | (Number _ as v)
  | (String _ as v) -> Ok (Cons (v, values))
  | l -> Error.cannot_execute l

and resolve closure = function
  | Symbol s -> Ok (World.get closure s)
  | l -> Ok l

and eval closure t =
  let eval_ = function
    | Cons (a, b) -> eval closure a >>= exec closure b
    | l -> resolve closure l
  in
  Trace.enter closure t >>= eval_ |> Trace.leave
