(* Printers for facts, ids, and frames.  Used when debugging *)

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

(* Printer for an id *)
let print_sym ctx f x =
  pp_print_string f (pname ctx x)

(* Facts *)

let print_commas pp f xs =
  let commas f () =
    pp_print_string f ",";
    pp_print_space f () in
  pp_print_list ~pp_sep:commas pp f xs

let print_args pp f xs =
  pp_print_string f "(";
  pp_open_box f 0;
  print_commas pp f xs;
  pp_print_string f ")";
  pp_close_box f ()

(* Printer for a fact *)
let print_fact ctx f = function
  | Q (s, []) ->
     print_sym ctx f s
  | Q (s, xs) ->
     print_sym ctx f s;
     print_args (print_sym ctx) f xs
  | G (s, [], a) ->
     print_sym ctx f s;
     pp_print_string f " = ";
     print_sym ctx f a
  | G (s, xs, a) ->
     pp_open_box f 2;
     print_sym ctx f s;
     print_args (print_sym ctx) f xs;
     pp_print_space f ();
     pp_print_string f "= ";
     print_sym ctx f a;
     pp_close_box f ()

(* Frames *)

(* Printer for a frame *)
let print_a_frame pr f {label; parent; structure; status; cause; _} =
  pp_open_box f 2;
  pp_print_string f "(";
  pp_print_int f label;
  (match parent with
   | None -> ()
   | Some p ->
      pp_print_string f ",";
      pp_print_int f p);
  pp_print_string f ")";
  (match cause with
   | None -> ()
   | Some c ->
      pp_print_string f "{";
      pp_print_int f c;
      pp_print_string f "}");
  if status = Sat then
    pp_print_string f "!"
  else if status = Aborted then
    pp_print_string f "?";
  pp_print_string f "[";
  pr @@ facts structure;
  pp_print_string f "]";
  pp_close_box f ();
  pp_print_newline f ()

(* Standard printer *)
let print_frame ctx f fr =
  let pr facts =
    print_commas (print_fact ctx) f facts in
  print_a_frame pr f fr

(* Print a frame with an unflattened structure *)

let rec print_term ctx f = function
  | C x -> print_sym ctx f x
  | V _ -> assert false
  | F (s, []) -> print_sym ctx f s
  | F (s, xs) ->
     print_sym ctx f s;
     print_args (print_term ctx) f xs

(* Printer for an atom *)
let print_atom ctx f = function
  | P (s, []) ->
     print_sym ctx f s
  | P (s, xs) ->
     print_sym ctx f s;
     print_args (print_term ctx) f xs
  | E (x, y) ->
     pp_open_box f 2;
     print_term ctx f x;
     pp_print_space f ();
     pp_print_string f "= ";
     print_term ctx f y;
     pp_close_box f ()
  | U x ->
     pp_open_box f 2;
     print_term ctx f x;
     pp_print_space f ();
     pp_print_string f "= ";
     print_term ctx f x;
     pp_close_box f ()

let print_unflattened_atoms ctx f facts =
  let atoms = unflatten facts in
  print_commas (print_atom ctx) f atoms

(* Print frame with an unflattened structure *)
let print_unflattened ctx f fr =
  print_a_frame (print_unflattened_atoms ctx f) f fr
