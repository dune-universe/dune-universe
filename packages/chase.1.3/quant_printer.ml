(* Printer for formulas using quantifiers.  Also used when debugging *)

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

let rec print_term fctx vctx f = function
  | C s -> pp_print_string f (pname vctx s)
  | V s -> pp_print_string f (pname vctx s)
  | F (s, []) ->
     pp_print_string f (pname fctx s)
  | F (s, x :: xs) ->
     pp_print_string f (pname fctx s);
     pp_print_string f "(";
     pp_open_box f 0;
     print_term fctx vctx f x;
     print_args fctx vctx f xs

and print_args fctx vctx f = function
  | [] ->
     pp_print_string f ")";
     pp_close_box f ()
  | x :: xs ->
     pp_print_string f ",";
     pp_print_space f ();
     print_term fctx vctx f x;
     print_args fctx vctx f xs

let print_atom fctx vctx f = function
  | P (s, []) ->
     print_sym fctx f s
  | P (s, x :: xs) ->
     print_sym fctx f s;
     pp_print_string f "(";
     pp_open_box f 0;
     print_term fctx vctx f x;
     print_args fctx vctx f xs
  | E (x, y) ->
     pp_open_box f 2;
     print_term fctx vctx f x;
     pp_print_space f ();
     pp_print_string f "= ";
     print_term fctx vctx f y;
     pp_close_box f ()
  | U _ ->                      (* This should never happen *)
     assert false

let rec print_vars vctx f = function
  | [] ->
     pp_print_string f ",";
     pp_close_box f ();
     pp_print_space f ()
  | x :: xs ->
     pp_print_space f ();
     pp_print_string f (pname vctx x);
     print_vars vctx f xs

let print_quantifier vctx quant f = function
  | [] -> ()
  | x :: xs ->
     pp_print_string f quant;
     pp_print_string f " ";
     pp_open_box f 0;
     pp_print_string f (pname vctx x);
     print_vars vctx f xs

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

let print_exists fctx vctx f {vars; form} =
  print_quantifier vctx "exists" f vars;
  print_conj fctx vctx f form

let print_disj fctx vctx f = function
  | [] -> pp_print_string f "false"
  | [x] -> print_exists fctx vctx f x
  | xs ->
     pp_open_box f 2;
     let sep f () =
       pp_print_space f ();
       pp_print_string f "| " in
     let quant f q =
       print_exists fctx vctx f q in
     pp_print_list ~pp_sep:sep quant f xs;
     pp_close_box f ()

let strip_conj (And conj) =
  let f = function
    | U _ -> false
    | _ -> true in
  And (List.filter f conj)

let strip_antec {vars; form} =
  { vars;
    form = strip_conj form;
  }

let print_antec fctx vctx f form =
  let {vars; form} = strip_antec form.antec in
  print_quantifier vctx "forall" f vars;
  match form with
   | And [] -> ()
   | And _ ->
      print_conj fctx vctx f form;
      pp_print_space f ();
      pp_print_string f "=> "

(* Print formula with quantifiers *)
let print_quant fctx f form =
  let vctx = mk_ctx () in
  pp_open_box f 2;
  print_antec fctx vctx f form;
  print_disj fctx vctx f form.concl;
  pp_print_string f ". % (";
  pp_print_int f form.tag;
  pp_print_string f ")";
  pp_close_box f ();
  pp_print_newline f ()
