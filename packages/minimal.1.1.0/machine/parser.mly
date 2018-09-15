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

%token <string> SYMBOL
%token <int64>  NUMBER
%token <string> STRING

%token TRUE
%token NIL
%token ANY
%token QUOTE
%token BACKQUOTE
%token TILDE
%token DOT
%token OPEN
%token CLOSE
%token EOF

%start <Grammar.t option> entry
%type  <Grammar.t>        cell
%type  <Grammar.t>        cells
%type  <Grammar.t>        escape
%%

entry:
  | c = cell { Some c }
  | EOF      { None   }

escape:
  | QUOTE;     c = cell { Grammar.Cons (Grammar.Symbol "quote", c) }
  | BACKQUOTE; c = cell {
    match Interpreter.eval Closure.empty c with
    | Ok c -> c
    | Error _ -> Nil
  }

lisp:
  | OPEN; c = cells; CLOSE { c }

cells:
  |                                      { Grammar.Nil         }
  | a = cell;        b = cells           { Grammar.Cons (a, b) }
  | a = cell; TILDE; b = cell; c = cells {
    match Interpreter.eval Closure.empty b with
    | Ok b -> Interpreter.conc c (Grammar.Cons (a, b))
    | Error _ -> Interpreter.conc c (Grammar.Cons (a, Nil))
  }
  | a = cell; DOT  ; b = cell            { Grammar.Cons (a, b) }

cell:
  |     TRUE   { Grammar.T        }
  |     NIL    { Grammar.Nil      }
  |     ANY    { Grammar.Any      }
  | n = NUMBER { Grammar.Number n }
  | y = SYMBOL { Grammar.Symbol y }
  | s = STRING { Grammar.String s }
  | e = escape { e                }
  | l = lisp   { l                }
