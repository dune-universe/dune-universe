(***************************************************************************)
(*                                 Morsmall                                *)
(*                      A concise AST for POSIX shell                      *)
(*                                                                         *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Ralf Treinen,          *)
(*  Nicolas Jeannerod                                                      *)
(*                                                                         *)
(*  This program is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation, either version 3 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *)
(***************************************************************************)

(** abstract syntax of test expressions *)

type expression =
  | And of expression * expression
  | Or  of expression * expression
  | Not of expression
  | Binary of string * string * string   (* (op,arg_left,arg_right) *)
  | Unary  of string * string            (* (op,arg) *)
  | Single of string                     (* arg *)

exception Parse_error

type token =
  | UnOp of string    (* unary operators -e, -f, etc. *)
  | BinOp of string   (* binary operators -eq, =, etc. *)
  | AndOp             (* -a *)
  | OrOp              (* -o *)
  | NotOp             (* ! *)
  | ParL              (* ( *)
  | ParR              (* ) *)
  | BracketR          (* ] *)
  | String of string  (* all the rest *)
  | EOF

let to_token s = match s with
  (* file existence and type *)
  | "-e" | "-d" | "-f" | "-b" | "-c" | "-h" | "-L" | "-p" | "-S" -> UnOp s
  (* file attributes *)
  | "-g" | "-u" | "-s" | "-r" | "-w" | "-x" -> UnOp s
  (* GNU extension on files *)
  | "-G" | "-O" | "-k" -> UnOp s
  (* GNU extension on files *)
  | "-nt" | "-ot" | "-ef" -> BinOp s
  (* unary operators on strings *)
  | "-n" | "-z" -> UnOp s
  (* binary operators on strings *)
  | "=" | "!=" -> BinOp s
  (* binary operators on integers *)
  | "-eq" | "-ne" | "-gt" | "-ge" | "-lt" | "-le" -> BinOp s
  (* unary operator on file descriptor *)
  | "-t" -> UnOp s
  | "-a" -> AndOp
  | "-o" -> OrOp
  | "("  -> ParL
  | ")"  -> ParR
  | "]"  -> BracketR
  | "!"  -> NotOp
  | _    -> String s

let parse ?(bracket=false) wl =
  let tokenbuf =
    wl
    |> List.map Morbig.remove_quotes
    |> List.map to_token
    |> ref
  in
  let lookup () =
    match !tokenbuf with
    | h::_ -> h
    | [] -> EOF
  in
  let pop () =
    match !tokenbuf with
    | _::r -> tokenbuf := r
    | [] -> assert false
  in

  let rec parse_S () =
    let exp = parse_S' () in
    if bracket then
      if lookup () = BracketR then
        pop ()
      else
        raise Parse_error;
    if lookup () = EOF then
      exp
    else
      raise Parse_error

  and parse_S' () =
    match lookup () with
    | EOF | BracketR -> None
    | _ -> Some (parse_disj ())

  and parse_disj () =
    let head = parse_conj () in
    match parse_disj' () with
    | None -> head
    | Some rest -> Or (head,rest)

  and parse_disj' () =
    match lookup () with
    | EOF | BracketR | ParR -> None
    | OrOp -> pop (); Some (parse_disj ())
    | _ -> raise Parse_error

  and parse_conj () =
    let head = parse_literal () in
    match parse_conj' () with
    | None -> head
    | Some rest ->  And (head, rest)

  and parse_conj' () =
    match lookup () with
    | OrOp | EOF | BracketR | ParR -> None
    | AndOp -> pop (); Some (parse_conj ())
    | _ -> raise Parse_error

  and parse_literal () =
    match lookup () with
    | NotOp -> pop (); Not (parse_atom ())
    | UnOp _ | ParL | String _ -> parse_atom ()
    | _ -> raise Parse_error

  and parse_atom () =
    match lookup () with
    | UnOp op ->
       pop ();
       (match lookup () with
        | String s -> pop (); Unary (op,s)
        | _ -> raise Parse_error)
    | ParL ->
       pop ();
       let exp = parse_disj () in
       (match lookup () with
        | ParR -> pop (); exp
        | _ -> raise Parse_error)
    | String s ->
       pop ();
       (match parse_atom' () with
        | None -> Single s
        | Some (binop,rightarg) -> Binary (binop,s,rightarg))
    | _ -> raise Parse_error

  and parse_atom' () =
    match lookup () with
    | AndOp | OrOp | EOF | BracketR -> None
    | BinOp binop ->
       pop ();
       (match lookup () with
        | String rightarg | UnOp rightarg | BinOp rightarg
          -> pop (); Some (binop,rightarg)
        | _ -> raise Parse_error)
    | _ -> raise Parse_error
  in

  parse_S ()


(*

grammar of test expressions:

<S>        -> EOF | <disj> EOF
<disj>     -> <conj> | <conj> -o <disj>
<conj>     -> <literal> | <literal> -a <conj>
<literal>  -> <atom> | ! <atom>
<atom>     -> string | unop string | string binop string | ( <disj> )

grammar in LL(1):

<S>        -> <S'> EOF
<S'>       -> EPSILON | <disj>
<disj>     -> <conj> <disj'>
<disj'>    -> EPSILON | -o <disj>
<conj>     -> <literal> <conj'>
<conj'>    -> EPSILON | -a <conj>
<literal>  -> <atom> | ! <atom>
<atom>     -> string <atom'> | unop string | ( <disj> )
<atom'>    -> EPSILON | binop string

annulating non-terminals: { <disj'>, <conj'>, <atom'> }

nonterminal | Fi_1
------------+--------------------
<S>         | string, unop, (, !
<disj>      | string, unop, (, !
<disj'>     | -o
<conj>      | string, unop, (, !
<conj'>     | -a
<literal>   | string, unop, (, !
<atom>      | string, unop, (
<atom'>     | binop

right side         | FIRST_1
-------------------+---------------------
<disj> EOF         | string, unop, (, !
<conj> <disj'>     | string, unop, (, !
-o <disj>          | -o
<literal> <conj'>  | string, unop, (, !
-a <conj>          | -a
<atom>             | string, unop, (
! <atom>           | !
string <atom'>     | string
unop string        | unop
( <disj> )         | (
binop string       | binop

nonterminal | FOLLOW_1
------------+--------------------
<S>         | \emptyset
<disj>      | EOF, )
<disj'>     | EOF, )
<conj>      | -o, EOF, )
<conj'>     | -o, EOF, )
<literal>   | -a, -o, EOF, )
<atom>      | -a, -o, EOF, )
<atom'>     | -a, -o, EOF, )

Hence we have the following requirements for being LL(1):

nonterminal | must be mutually disjoint
------------+--------------------------
<S>         | ---
<disj>      | ---
<disj'>     | EOF, ), -o
<conj>      | ---
<conj'>     | -o, EOF, ), -a
<literal>   | string, unop, (, !
<atom>      | string, unop, (
<atom'>     | -a, -o, EOF, ), binop

*)
