(* Checks a formula.  Ensures that an unused variable does not occur.
   A variable is unused if it only occurs in an atomic formula that
   equates two variables. *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Sym
open Formula

let rec cited_term var = function
  | C _ -> false
  | V v -> same var v
  | F (_, ts) -> cited_terms var ts
and cited_terms var ts =
  List.exists (cited_term var) ts

let cited_eq var x y =
  match x, y with
  | V _, V _ -> false
  | V _, _ -> cited_term var y
  | _, V _ -> cited_term var x
  | _ -> cited_term var x || cited_term var y

let cited_atom var = function
  | P (_, ts) -> cited_terms var ts
  | E (x, y) -> cited_eq var x y
  | U x -> cited_term var x

let cited_conj var conj =
  List.exists (cited_atom var) conj

let check_var vars conj var =
  mem var vars || cited_conj var conj

let check_eq vars conj = function
  | E (V x, V y) ->
     check_var vars conj x && check_var vars conj y
  | _ -> true

let check_quant vars {form = And conj; _} =
  not @@ List.for_all (check_eq vars conj) conj

let check form =
  let msg = "Bad formula: unused variable in equation" in
  if check_quant [] form.antec then
    failwith (Reader.error_msg form.pos msg);
  let f conj =                  (* Check conclusion *)
    if check_quant form.antec.vars conj then
      failwith (Reader.error_msg form.pos msg) in
  List.iter f form.concl
