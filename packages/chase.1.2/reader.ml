(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)
(* Parse input and convert it into formulas *)

open Lexing
open Prelude
open Sym
open Ast
open Formula

(* Read file and return its contents as an abstract syntax tree. *)

(* Error messages that make use of file positions *)

let pos_info pos =
  pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1

let error_msg pos msg =
  let fname, lnum, cnum = pos_info pos in
  Printf.sprintf "%s:%d:%d: %s" fname lnum cnum msg

let parse_err pos msg =
  failwith (error_msg pos msg)

let read_lexbuf fname ch =
  let lexbuf = from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

let read_ast lexbuf =
  try
    Parser.file Scanner.token lexbuf
  with
  | Parsing.Parse_error ->
     parse_err (lexeme_start_p lexbuf) "Syntax error"

(* Convert an AST into a formula *)

let sym tbl s =
  match Hashtbl.find_opt tbl s with
  | None ->
     let x = mk_sym s in
     Hashtbl.add tbl s x;
     x
  | Some x -> x

let rec from_term fun_tbl var_tbl (Term (Sym (pos, s), args)) =
  if is_upper s then
    if args = [] then
      V (sym var_tbl s)
    else
      parse_err pos ("Variable cannot be used as a function symbol: " ^ s)
  else
    F (fun_tbl s, List.map (from_term fun_tbl var_tbl) args)

let from_atom fun_tbl var_tbl = function
  | Atom (Sym (pos, s), args) ->
     if is_upper s then
       parse_err pos ("Variable cannot be used as a predictate symbol: " ^ s)
     else
       P (fun_tbl s, List.map (from_term fun_tbl var_tbl) args)
  | Equal (x, y) ->
     E (from_term fun_tbl var_tbl x, from_term fun_tbl var_tbl y)

let from_conj fun_tbl var_tbl (Conj xs) =
  And (List.map (from_atom fun_tbl var_tbl) xs)

(* Ensure each disjunct has distinct variables *)
let from_each_conj fun_tbl var_tbl conj =
  from_conj fun_tbl (Hashtbl.copy var_tbl) conj

let from_disj fun_tbl var_tbl (Disj xs) =
  List.map (from_each_conj fun_tbl var_tbl) xs

let add_univ (And conj) =
  let f = function
    | E (V x, V y) when same x y ->
       U (V x)
    | x -> x in
  And (List.map f conj)

(* Counter used to number formulas.  The number is called a tag. *)

let counter = ref 0
let next () =
  let n = !counter in
  incr counter;
  n

let from_form fun_tbl (Form (p, c, d)) =
  let var_tbl = Hashtbl.create 16 in
  let conj = add_univ @@ from_conj fun_tbl var_tbl c in
  let disj = from_disj fun_tbl var_tbl d in
  let tag = next () in
  mk_form p tag conj disj

let read_file fname ch =
  let lexbuf = read_lexbuf fname ch in
  let fun_tab = sym (Hashtbl.create 256) in
  let bnd, lmt, ast = read_ast lexbuf in
  bnd, lmt, List.map (from_form fun_tab) ast
