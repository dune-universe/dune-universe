(* Printer for formulas.  Also used when debugging *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Format
open Sym
open Formula

(* Printer for a symbol *)
let print_sym ctx f x =
  pp_print_string f (pname ctx x)

let print_args pp f xs =
  pp_print_string f "(";
  pp_open_box f 0;
  let sep f () =
    pp_print_string f ",";
    pp_print_space f () in
  pp_print_list ~pp_sep:sep pp f xs;
  pp_print_string f ")";
  pp_close_box f ()

let rec print_term fctx vctx f = function
  | C s -> pp_print_string f (pname vctx s)
  | V s -> pp_print_string f (pname vctx s)
  | F (s, []) ->
     pp_print_string f (pname fctx s)
  | F (s, xs) ->
     pp_print_string f (pname fctx s);
     print_args (print_term fctx vctx) f xs

let print_atom fctx vctx f = function
  | P (s, []) ->
     print_sym fctx f s
  | P (s, xs) ->
     print_sym fctx f s;
     print_args (print_term fctx vctx) f xs
  | E (x, y) ->
     pp_open_box f 2;
     print_term fctx vctx f x;
     pp_print_space f ();
     pp_print_string f "= ";
     print_term fctx vctx f y;
     pp_close_box f ()
  | U x ->
     pp_open_box f 2;
     print_term fctx vctx f x;
     pp_print_space f ();
     pp_print_string f "= ";
     print_term fctx vctx f x;
     pp_close_box f ()

let print_conj fctx vctx f = function
  | And [] ->
     pp_print_string f "true"
  | And [x] ->
     print_atom fctx vctx f x
  | And xs ->
     pp_open_box f 2;
     let sep f () =
       pp_print_space f ();
       pp_print_string f "& " in
     pp_print_list ~pp_sep:sep (print_atom fctx vctx) f xs;
     pp_close_box f ()

let print_disj fctx vctx f = function
  | [] ->
     pp_print_string f "false"
  | [x] ->
     print_conj fctx vctx f x.form
  | xs ->
     pp_open_box f 2;
     let sep f () =
       pp_print_space f ();
       pp_print_string f "| " in
     let quant f q =
       print_conj fctx vctx f q.form in
     pp_print_list ~pp_sep:sep quant f xs;
     pp_close_box f ()

(* Printer for a formula *)
let print_form fctx f form =
  let vctx = mk_ctx () in
  pp_open_box f 2;
  (match form.antec.form with
   | And [] -> ()        (* Don't print antecedent when it is true *)
   | _ ->
      print_conj fctx vctx f form.antec.form;
      pp_print_space f ();
      pp_print_string f "=> ");
  print_disj fctx vctx f form.concl;
  pp_print_string f ". % (";
  pp_print_int f form.tag;
  pp_print_string f ")";
  pp_close_box f ();
  pp_print_newline f ()
