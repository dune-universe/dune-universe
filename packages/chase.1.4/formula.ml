(* Geometric formulas *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Sym

type term =
  | C of sym                    (* Structure constant *)
  | V of sym                    (* Variable *)
  | F of sym * term list        (* Function application *)

(* Is term an application of a function? *)
let funapp = function
  | F (_, _ :: _) -> true
  | _ -> false

(* An atomic formula *)
type atom =
  | P of sym * term list
  | E of term * term
  | U of term

(* A conjunction of atomic formulas *)
type conj = And of atom list

(* A conjunction and the variables quantified.  For the antecedent,
   the quantified variables are the ones that occur in the
   conjunction.  For a disjunct in the conclusion, the quantified
   variables are the ones that occur in the conjunction but do not
   occur in the antecedent. *)
type quant = {
    vars: sym list;
    form: conj;
  }

(* A formula in geometric form *)
type form = {
    pos: Lexing.position;       (* Position of '=>' in input file *)
    tag: int;                   (* Number that identifies formula *)
    antec: quant;               (* Antecedent *)
    concl: quant list;          (* Consequent *)
    mutable enabled : bool;     (* Flag used by Solve module *)
  }

(* A list of formulas and their constants *)
type axioms = {
    consts: sym list;
    forms: form list;
  }

(* Fold through terms in a formula *)

let fold_atom f atom a =
  match atom with
  | P (_, xs) ->
     List.fold_right f xs a
  | E (x, y) ->
     f y (f x a)
  | U x -> f x a

let fold_conj f (And xs) a =
  List.fold_right (fold_atom f) xs a

let fold_quant f {form; _} a =
  fold_conj f form a

let fold_form f {antec; concl; _} a =
  fold_quant f antec (List.fold_right (fold_quant f) concl a)

(* Fold through variables in formulas *)

let rec fold_var_term f form a =
  match form with
  | C _ -> a
  | V x -> f x a
  | F (_, xs) ->
     List.fold_right (fold_var_term f) xs a

let free_vars conj =
  fold_conj (fold_var_term add) conj []

(* Fold through constants in formulas *)

let rec fold_const_term f form a =
  match form with
  | C _ -> a
  | V _ -> a
  | F (x, []) -> f x a
  | F (_, xs) ->
     List.fold_right (fold_const_term f) xs a

let fold_const_form form a =
  fold_form (fold_const_term add) form a

(* Make a formula *)

let mk_form pos tag antec concl =
  let uni = free_vars antec in
  let f conj = {
      vars = diff (free_vars conj) uni;
      form = conj } in
  { pos;
    tag;
    antec = {
        vars = uni;
        form = antec
      };
    concl = List.map f concl;
    enabled = true }

(* Is the antecedent empty? *)
let no_antec { antec; _ } =
  match antec.form with
  | And [] -> true
  | _ -> false

(* Is there at most one disjunct and no existentally quantified
   variables? *)
let is_lightweight { concl; _ } =
  match concl with
  | [] -> true
  | [q] -> q.vars = []
  | _ -> false

(* Make a set of axioms *)

let mk_axioms forms = {
    consts = List.fold_right fold_const_form forms [];
    forms
  }

(* Substitution *)

type subst = (sym * term) list

let in_dom x sb =
  let f (y, _) = same x y in
  List.exists f sb

let rec subst_term sb = function
  | C _ as x -> x
  | V x ->
     (match assoc_opt x sb with
      | None -> V x
      | Some y -> y)
  | F (s, xs) ->
     F (s, List.map (subst_term sb) xs)

let subst_atom sb = function
  | P (s, xs) ->
     P (s, List.map (subst_term sb) xs)
  | E (x, y) ->
     E (subst_term sb x,
        subst_term sb y)
  | U x -> U (subst_term sb x)

(* Apply a substitution to a conjunction of atomic formulas *)
let subst_conj sb (And xs) =
  And (List.map (subst_atom sb) xs)
