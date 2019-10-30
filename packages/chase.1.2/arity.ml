(* Arity check *)

(* Ensure predicates and functions are used at one arity. *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Sym
open Formula
open Reader

type kind = Pred | Func

let show_kind = function
  | Pred -> "Predicate"
  | Func -> "Function"

let show_an_error symbol kind pos =
  error_msg pos @@
    Printf.sprintf "%s %s used at more than one arity"
      (show_kind kind) (string_of_sym symbol)

let show_a_kind_error symbol pos =
  error_msg pos @@
    Printf.sprintf "%s used as both a predicate and a function symbol"
      (string_of_sym symbol)

let show_kind_error symbol pos p =
  if p = pos then
    failwith @@ show_a_kind_error symbol pos
  else
    failwith (show_a_kind_error symbol p ^ "\n"
              ^ show_a_kind_error symbol pos)

let show_error symbol kind k pos p =
  if not (kind = k) then
    show_kind_error symbol pos p
  else if p = pos then
    failwith @@ show_an_error symbol kind pos
  else
    failwith (show_an_error symbol k p ^ "\n"
              ^ show_an_error symbol kind pos)

let rec check_term syms pos = function
  | F (s, xs) ->
     let n = List.length xs in
     (match SymHashtbl.find_opt syms s with
      | None -> SymHashtbl.add syms s (Func, n, pos)
      | Some (Func, m, _) when m = n -> ()
      | Some (k, _, p) -> show_error s Func k pos p);
     List.iter (check_term syms pos) xs
  | _ -> ()

let check_atom syms pos = function
  | P (s, xs) ->
     let n = List.length xs in
     (match SymHashtbl.find_opt syms s with
      | None -> SymHashtbl.add syms s (Pred, n, pos)
      | Some (Pred, m, _) when m = n -> ()
      | Some (k, _, p) -> show_error s Pred k pos p);
     List.iter (check_term syms pos) xs
  | E (x, y) ->
     check_term syms pos x;
     check_term syms pos y
  | U x ->
     check_term syms pos x

let check_conj syms pos (And conj) =
  List.iter (check_atom syms pos) conj

let check_quant syms pos quant =
  check_conj syms pos quant.form

let check_form syms form =
  check_quant syms form.pos form.antec;
  List.iter (check_quant syms form.pos) form.concl

let arity forms =
  let syms = SymHashtbl.create 60 in
  List.iter (check_form syms) forms
