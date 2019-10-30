(* Flattening tranformation *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Sym
open Formula

(* On input, the flattener expects a formula without unused variables.
   A variable is unused if it only occurs in an atomic formula that
   equates two variables.  The Check module ensures this. *)

(* On output, the flattener produces a logically equivalent formula in
   which atomic formulas have one of three forms:

   P(X1, ... , Xn) where P is a predicate symbol
                   and X1, ... , Xn are distinct variables.

   f(X1, ... , Xn) = X0, where f is a function symbol
                         and X0, ... , Xn are distinct variables.

   X = Y where X and Y are different variables.

   Whenever a variable is eliminated in favor of another, the variable
   with the largest integer is eliminated.
 *)

(* There are three flattening phases.  The first phase creates a table
   that maps each non-variable term to a fleshly allocated variable,
   and replaces the term in the formula by the variable.  The
   equations in the table and the formula are logically equivalent to
   the input formula, however every equality in the original formula
   is tranformed into an equality between variables.  The second phase
   of flattening uses substitution to eliminate equalities between
   variables except it the case in which the variables occur in the
   antecedent and the equality is in the conclusion.  The final phase
   ensures that variables are distinct in all atomic formulas. *)

let flattener_var = "C"  (* Root name for freshly created variables *)

(* A flattening table is a map from terms that are function applications to
   symbols that represent variables. *)

(* This routine extracts the binding as an equation. *)
let bindings tbl =
  let f t v c =
    E (t, V v) :: c in
  Hashtbl.fold f tbl []

(* Flatten term always returns a variable *)
let rec flatten_term tbl = function
  | C _ -> assert false         (* Constants are only in structures *)
  | V _ as x -> x
  | F (s, xs) ->
     let ys = List.map (flatten_term tbl) xs in
     let x = F (s, ys) in
     match Hashtbl.find_opt tbl x with
     | None ->
        let y = mk_sym flattener_var in
        Hashtbl.add tbl x y;
        V y
     | Some y -> V y

let flatten_atom tbl = function
  | P (s, xs) ->
     P (s, List.map (flatten_term tbl) xs)
  | E (x, y) ->
     E (flatten_term tbl x, flatten_term tbl y)
  | U x ->
     U (flatten_term tbl x)

let flatten_conj tbl (And xs) =
  let conj = List.map (flatten_atom tbl) xs in
  And (bindings tbl @ conj)

let flatten_disj (And antec) tbl {form = conj; _} =
  let tbl = Hashtbl.copy tbl in
  let (And flat) = flatten_conj tbl conj in
  let f a = not (List.mem a antec) in
  And (List.filter f flat)

let flatten_form form =
  let tbl = Hashtbl.create 32 in
  let antec = flatten_conj tbl form.antec.form in
  mk_form
    form.pos
    form.tag
    antec
    (List.map (flatten_disj antec tbl) form.concl)

(* Simplify *)

(* Substitute in a symbol. *)
let rec subst sb x =
  match sb with
    [] -> x
  | (y, z) :: _ when same x y -> z
  | _ :: sb -> subst sb x

(* Substitute in a term by updating variables *)
let rec subst_term sb = function
  | C _ -> assert false
  | V x -> V (subst sb x)
  | F (s, xs) -> F (s, List.map (subst_term sb) xs)

let subst_atom sb = function
  | P (s, xs) ->
     P (s, List.map (subst_term sb) xs)
  | E (x, y) ->
     E (subst_term sb x, subst_term sb y)
  | U x ->
     U (subst_term sb x)

let subst_conj sb (And conj) =
  And (List.map (subst_atom sb) conj)

(* Add a maplet to a substitution *)
let rec add_subst sb x y =
  if int_of_sym x < int_of_sym y then (* Eliminate variables *)
    add_subst sb y x                  (* with larger integer *)
  else
    let z = subst sb y in
    if same x z then            (* Avoid loops *)
      sb
    else
      let maplet = [x, z] in    (* Apply maplet to range of sb *)
      let f (x, y) = x, subst maplet y in
      (x, z) :: List.map f sb

let mem_sym x xs =
  List.exists (same x) xs

(* Eliminate an equation except when the variables occur in the
   antecedent and the equation occurs in the conclusion. *)
let extend_subst vars sb (And conj) =
  let f sb = function
    | E (V x, V y)
         when not (mem_sym x vars && mem_sym x vars) ->
       add_subst sb x y
    | _ -> sb in
  List.fold_left f sb conj

(* Handle the stupid case in which the conclusion equates a variable in
   the antecedent to itself. *)
let rm_reflexive (And conj) =
  let f = function
    | E (V x, V y) when same x y -> false
    | _ -> true in
  And (List.filter f conj)

let simplify form =
  let sb = extend_subst [] [] form.antec.form in
  let antec = rm_reflexive @@ subst_conj sb form.antec.form in
  let simplify_disj {form = conj; _}  =
    let sb = extend_subst form.antec.vars sb conj in
    rm_reflexive @@ subst_conj sb conj in
  mk_form
    form.pos
    form.tag
    antec
    (List.map simplify_disj form.concl)

(* Linearization *)

let args xs =
  let rec loop set ys eqs = function
    | [] -> List.rev ys, List.rev eqs
    | V x :: xs when mem x set ->
       let z = clone x in
       loop set (V z :: ys) (E (V z, V x) :: eqs) xs
    | V x :: xs -> loop (add x set) (V x :: ys) eqs xs
    | C x :: xs -> loop set (C x :: ys) eqs xs
    | F _ :: _ -> assert false in
  loop [] [] [] xs

let linearize_atom = function
  | P (s, xs) ->
     let ys, eqs = args xs in
     P (s, ys) :: eqs
  | E (F (s, xs), V z) ->
     let ys, eqs = args (V z :: xs) in
     E (F (s, List.tl ys), List.hd ys) :: eqs
  | atom -> [atom]

let linearize_conj (And conj) =
  And (List.concat @@ List.map linearize_atom conj)

let linearize_quant {vars = _; form} =
  linearize_conj form

let linearize form =
  mk_form
    form.pos
    form.tag
    (linearize_quant form.antec)
    (List.map linearize_quant form.concl)

(* Flattening entry point *)
let flatten form =
  linearize @@ simplify @@ flatten_form @@ form
