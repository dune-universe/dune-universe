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

let name = "out"

let regex = Str.regexp "^\\+.*$"

let in_file closure mode prg filename =
  try
    let old = !Interpreter.out_channel in
    let dsc = Unix.openfile filename mode 0o640 in
    let chn = Unix.out_channel_of_descr dsc in
    Interpreter.out_channel := (filename, chn);
    begin try
        let res = Interpreter.eval closure prg in
        Interpreter.out_channel := old;
        flush chn;
        Unix.close dsc;
        res
      with e ->
        Interpreter.out_channel := old;
        flush chn;
        Unix.close dsc;
        raise e
    end
  with Unix.Unix_error _ ->
    Printf.printf "Exception !!\n";
    Ok Nil

let process closure = function
  | Nil, Cons (prg, Nil) ->
    let old = !Interpreter.out_channel in
    Interpreter.out_channel := ("stdout", stdout);
    begin try
        let res = Interpreter.eval closure prg in
        Interpreter.out_channel := old;
        flush stdout;
        res
      with e ->
        Interpreter.out_channel := old;
        flush stdout;
        raise e
    end
  | String fn, Cons (prg, Nil) when Str.string_match regex fn 0 && String.length fn > 1 ->
    String.sub fn 1 ((String.length fn) - 1)
    |> in_file closure [ Unix.O_WRONLY; Unix.O_APPEND ] prg
  | String fn, Cons (prg, Nil) when String.length fn > 0 ->
    in_file closure [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] prg fn
  | a, b -> Error.undefined (Cons (a, b))

let run closure = function
  | Cons (fn, prg) ->
    Interpreter.eval closure fn >>= fun fn -> process closure (fn, prg)
  | t -> Error.undefined t

let hook = (name, run)
