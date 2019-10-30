(* Parse input and convert it into formulas *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Lexing
open Prelude
open Sym
open Quant_ast
open Formula

(* Read file and return its contents as an abstract syntax tree. *)

(* Error messages that make use of file positions *)

let parse_err pos msg =
  failwith (Reader.error_msg pos msg)

let read_lexbuf fname ch =
  let lexbuf = from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

let read_ast lexbuf =
  try
    Quant_parser.file Quant_scanner.token lexbuf
  with
  | Parsing.Parse_error ->
     parse_err (lexeme_start_p lexbuf) "Syntax error"

(* Validate the input *)

(* Ensure variables are not distinguished by capitalization. *)

let collect_quant acc (Quant (vars, _)) =
  let f acc (Sym (_, var)) =
    if List.mem var acc then
      acc
    else
      var :: acc in
  List.fold_left f acc vars

(* Construct the set of quantified variables *)
let collect_vars (Form (_, antec, Disj concl)) =
  let avars = collect_quant [] antec in
  List.fold_left collect_quant avars concl

let check_for_dups (Form (pos, _, _) as form) =
  let vars = collect_vars form in
  let vars = List.map cap vars in (* Capitalize all variables  *)
  let rec loop = function         (* Look for duplicates *)
    | [] -> ()                    (* No duplicates found *)
    | v :: vs ->
       if List.mem v vs then
         parse_err pos          (* Duplicate found *)
           ("Quantified variable occurs at two cases: " ^ v)
       else
         loop vs in
  loop vars

(* Convert a validated AST into a formula *)

let sym tbl s =
  match Hashtbl.find_opt tbl s with
  | None ->
     let x = mk_sym s in
     Hashtbl.add tbl s x;
     x
  | Some x -> x

(* Convert a validated AST into a formula *)

let rec from_term tbl env (Term (Sym (pos, s), args)) =
  match List.assoc_opt s env with
  | Some sym ->
     if args = [] then
       V sym                    (* Symbol is a variable *)
     else
       parse_err pos ("Variable cannot be used as a function symbol: " ^ s)
  | None ->
     if is_upper s then
       parse_err pos ("Function symbol cannot be capitalized: " ^ s)
     else
       F (tbl s, List.map (from_term tbl env) args)

let from_atom tbl env = function
  | Atom (Sym (pos, s), args) ->
     if is_upper s then
       parse_err pos ("Variable cannot be used as a predictate symbol: " ^ s)
     else
       P (tbl s, List.map (from_term tbl env) args)
  | Equal (x, y) ->
     E (from_term tbl env x, from_term tbl env y)

let from_conj tbl env (Quant (vars, Conj xs)) =
  let f (Sym (_, s)) = s, mk_sym @@ cap s in
  let env = List.map f vars @ env in
  env, And (List.map (from_atom tbl env) xs)

let from_disj tbl env (Disj xs) =
  let f x = snd @@ from_conj tbl env x in
  List.map f xs

(* Add atoms for universally quantified variables in the consequent. *)

let univ vars (And conj) =
  let free = free_vars (And conj) in
  let unies = diff vars free in
  let f sym = U (V sym) in
  if unies = [] then
    And conj
  else
    And (conj @ List.map f unies)

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

let from_form tbl (Form (p, c, d)) =
  let env, conj = from_conj tbl [] c in
  let disj = from_disj tbl env d in
  let conj = univ (List.map snd env) conj in
  let tag = next () in
  mk_form p tag (add_univ conj) disj

let read_file fname ch =
  let lexbuf = read_lexbuf fname ch in
  let fun_tab = sym (Hashtbl.create 256) in
  let bnd, lmt, ast = read_ast lexbuf in
  List.iter check_for_dups ast;
  bnd, lmt, List.map (from_form fun_tab) ast
