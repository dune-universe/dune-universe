(* Chase step *)

(* Given a structure and a formula, the step function takes a chase
   step.  It returns None if the structure satisfies the formula.
   Otherwise it returns a list of structures that have been extended
   so as to satisfy the formula. *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Prelude
open Sym
open Formula
open Match
open Structure

(* Substitute constants into formula. *)
let subst_form env form =
  subst_conj (List.map (fun (x, y) -> x, C y) env) form

(* Create a new structure for one of the disjuncts *)
let chase stc sb {vars; form} =
  let f id env = (id, mk_sym (uncap (string_of_sym id))) :: env in
  (* Allocate a constant for each existentially quantified variable. *)
  let sb = List.fold_right f vars sb in
  let And nform = subst_form sb form in (* Apply substitution *)
  augment_struct stc nform  (* Augment structure with new atoms *)

(* See if one of the disjuncts is satisfied with substitution sb. *)
let rec check_conseq stc rule next sb = function
  | [] ->                     (* None of the disjuncts is satisfied *)
     Some (List.map (chase stc sb) rule.concl)
  | {vars = _; form} :: forms ->
     let nform = subst_form sb form in
     match match_conj stc nform with
     | Nil ->                   (* Disjunct not satisfied *)
        check_conseq stc rule next sb forms
     | Cons _ ->                (* Model satisfies formula *)
        match next () with
        | Nil -> None
        | Cons (sb, next) -> (* Try another substitution that satisfies *)
           check_conseq stc rule next sb rule.concl (* the antecedent *)

(* Take a chase step. *)
let step stc rule =
  let body = match_conj stc rule.antec.form in
  match body with
  | Nil -> None                 (* Model satisfies formula *)
  | Cons (sb, next) ->          (* Antecedent is satisfied with *)
     check_conseq stc rule next sb rule.concl (* substitution sb *)
