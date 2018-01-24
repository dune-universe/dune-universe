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

let functions = [
  Functions.Add.hook;
  Functions.And.hook;
  Functions.Car.hook;
  Functions.Catch.hook;
  Functions.Cdr.hook;
  Functions.Case.hook;
  Functions.Conc.hook;
  Functions.Cons.hook;
  Functions.Def.hook;
  Functions.Div.hook;
  Functions.Equ.hook;
  Functions.Env.hook;
  Functions.Eval.hook;
  Functions.Flush.hook;
  Functions.Funp.hook;
  Functions.Ge.hook;
  Functions.Gt.hook;
  Functions.If.hook;
  Functions.In.hook;
  Functions.Join.hook;
  Functions.Le.hook;
  Functions.Let.hook;
  Functions.Line.hook;
  Functions.List.hook;
  Functions.Load.hook;
  Functions.Lstp.hook;
  Functions.Lt.hook;
  Functions.Mul.hook;
  Functions.Ne.hook;
  Functions.Nilp.hook;
  Functions.Not.hook;
  Functions.Nump.hook;
  Functions.Or.hook;
  Functions.Out.hook;
  Functions.Prin.hook;
  Functions.Prinl.hook;
  Functions.Print.hook;
  Functions.Println.hook;
  Functions.Prog.hook;
  Functions.Quit.hook;
  Functions.Quote.hook;
  Functions.Read.hook;
  Functions.Setq.hook;
  Functions.Split.hook;
  Functions.Strp.hook;
  Functions.Sub.hook;
  Functions.Sym.hook;
  Functions.Symp.hook;
  Functions.Throw.hook;
  Functions.Trace.hook;
  Functions.Unless.hook;
  Functions.When.hook;
  Functions.While.hook;
]

let register (name, fn) =
  World.set name (Grammar.Function (name, fn))

(*
 * Prefix management.
 *)

let set_prefix p =
  p |> Filename.dirname |> Filename.dirname |> Interpreter.set_prefix

(*
 * Main.
 *)

let args = Array.to_list Sys.argv

let rec build_args args =
  args
  |> ListLabels.fold_right ~f:(fun e acc -> Cons (String e, acc)) ~init:Nil
  |> World.set "args"

let () =
  List.iter register functions;
  match args with
  | [ _ ] ->
    set_prefix Sys.executable_name;
    Frontend.repl ()
  | _ :: ((fn :: _) as args) ->
    build_args args;
    set_prefix Sys.executable_name;
    Frontend.load fn |> ignore
  | _ -> ()
