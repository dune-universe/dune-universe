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

let name = "λ"

module Args = Set.Make(String)

let rec define args set =
  match args with
  | Symbol s -> Args.add s set
  | Cons (a, b) -> set |> define a |> define b
  | _ -> set

let rec collect closure excl args body c =
  match body with
  | Symbol s when
      not (ListLabels.exists ~f:(fun e -> String.compare s e = 0) excl) ->
    begin match Args.find_opt s args with
      | Some s -> c
      | None -> Closure.add s (World.get closure s) c
    end
  | Cons (a, b) ->
    c |> collect closure excl args a |> collect closure excl args b
  | _ -> c

let mkfun closure name args body excl =
  let limits = define args Args.empty in
  let closure = collect closure excl limits body Closure.empty in
  Function (args, body, closure)

let rec run closure = function
  | Cons (Nil, Cons (Cons _ as body, Nil)) ->
    Ok (mkfun closure "λ" Nil body [ ])
  | Cons (Cons _ as args, Cons (Cons _ as body, Nil)) ->
    Ok (mkfun closure "λ" args body [ ])
  | t -> Error.undefined t

let hook = (name, run)
