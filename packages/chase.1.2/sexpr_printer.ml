(* A printer for frames that produces output in S-expression syntax *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Format
open Sym
open Fact
open Structure
open Solve
open Formula
open Unflatten

let print_sym ctx f x =
  pp_print_string f (pname ctx x)

let rec print_syms ctx f = function
  | [] ->
     pp_print_string f ")";
     pp_close_box f ()
  | x :: xs ->
     pp_print_space f ();
     print_sym ctx f x;
     print_syms ctx f xs

let print_fact ctx f = function
  | Q (s, []) ->
     pp_print_string f "(";
     print_sym ctx f s;
     pp_print_string f ")";
  | Q (s, xs) ->
     pp_open_box f 2;
     pp_print_string f "(";
     print_sym ctx f s;
     print_syms ctx f xs
  | G (s, [], a) ->
     pp_print_string f "(= ";
     print_sym ctx f s;
     pp_print_string f " ";
     print_sym ctx f a;
     pp_print_string f ")";
  | G (s, xs, a) ->
     pp_open_box f 3;
     pp_print_string f "(= (";
     pp_open_box f 1;
     print_sym ctx f s;
     print_syms ctx f xs;
     pp_print_space f ();
     print_sym ctx f a;
     pp_print_string f ")";
     pp_close_box f ()

(* Print a frame *)

let rec print_facts ctx f = function
  | [] -> ()
  | x :: xs ->
     pp_print_space f ();
     print_fact ctx f x;
     print_facts ctx f xs

let print_frame pr f {label; parent; structure; status; cause; _} =
  pp_open_box f 2;
  pp_print_string f "(struct";
  pp_print_space f ();
  pp_print_string f "(label ";
  pp_print_int f label;
  pp_print_string f ")";
  (match parent with
   | None -> ()
   | Some p ->
      pp_print_space f ();
      pp_print_string f "(parent ";
      pp_print_int f p;
      pp_print_string f ")");
  (match cause with
   | None -> ()
   | Some c ->
      pp_print_space f ();
      pp_print_string f "(sequent ";
      pp_print_int f c;
      pp_print_string f ")");
  (match status with
   | Unsat -> ()
   | Sat ->
      pp_print_space f ();
      pp_print_string f "(status model)";
   | Aborted ->
      pp_print_space f ();
      pp_print_string f "(status aborted)");
  pp_print_space f ();
  pp_open_box f 2;
  pp_print_string f "(facts";
  pp_print_space f ();
  pr @@ facts structure;
  pp_print_string f "))";
  pp_close_box f ();
  pp_close_box f ();
  pp_print_newline f ()

let print_sexpr_frame ctx f fr =
  let pr facts =
    print_facts ctx f facts in
  print_frame pr f fr

let print_spaces pp f xs =
  pp_print_list ~pp_sep:pp_print_space pp f xs

let rec print_term ctx f = function
  | C x -> print_sym ctx f x
  | V _ -> assert false
  | F (s, []) -> print_sym ctx f s
  | F (s, xs) ->
     pp_open_box f 2;
     pp_print_string f "(";
     print_sym ctx f s;
     pp_print_space f ();
     print_spaces (print_term ctx) f xs;
     pp_print_string f ")";
     pp_close_box f ()

let print_atom ctx f = function
  | P (s, []) ->
     pp_print_string f "(";
     print_sym ctx f s;
     pp_print_string f ")"
  | P (s, xs) ->
     pp_open_box f 2;
     pp_print_string f "(";
     print_sym ctx f s;
     pp_print_space f ();
     print_spaces (print_term ctx) f xs;
     pp_print_string f ")";
     pp_close_box f ()
  | E (x, y) ->
     pp_open_box f 2;
     pp_print_string f "(=";
     pp_print_space f ();
     print_term ctx f x;
     pp_print_space f ();
     print_term ctx f y;
     pp_print_string f ")";
     pp_close_box f ()
  | U x ->
     pp_open_box f 2;
     pp_print_string f "(=";
     pp_print_space f ();
     print_term ctx f x;
     pp_print_space f ();
     print_term ctx f x;
     pp_print_string f ")";
     pp_close_box f ()

let print_unflattened_atoms ctx f facts =
  let atoms = unflatten facts in
  print_spaces (print_atom ctx) f atoms

(* Print frame with an unflattened structure *)
let print_sexpr_unflattened ctx f fr =
  print_frame (print_unflattened_atoms ctx f) f fr
