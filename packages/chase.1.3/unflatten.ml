(* Convert a structure into a conjunction of atoms and then eliminate
   unnecessary constants by substituting using equality. *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

(* In this module, terms never contain variables *)

open Sym
open Formula
open Fact

(* Are two terms equal? *)
let rec eq x y =
  match x, y with
  | C x, C y -> same x y
  | V x, V y -> same x y
  | F (s, xs), F (t, ys) when same s t ->
     eqs xs ys
  | _ -> false
and eqs xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xs, y :: ys when eq x y -> eqs xs ys
  | _, _ -> false

let term x = C x

(* Convert a fact into an atom. *)
let transform = function
  | Q (s, xs) -> P (s, List.map term xs)
  | G (s, xs, x) -> E (F (s, List.map term xs), term x)

(* Substitition into a term *)
let rec subst sb = function
  | C x as t ->
     (match assoc_opt x sb with
      | None -> t
      | Some t -> t)
  | V _ -> assert false
  | F (s, xs) ->
     F (s, List.map (subst sb) xs)

(* Substitition into an atom *)
let subst_atom sb = function
  | P (s, xs) -> P (s, List.map (subst sb) xs)
  | E (x, y) -> E (subst sb x, subst sb y)
  | U x -> U (subst sb x)

(* Does x occur in term t? *)
let rec occurs x = function
  | C y -> same x y
  | V _ -> assert false
  | F (_, xs) -> List.exists (occurs x) xs

(* Make a substitution without cycles *)
let rec make_subst = function
  | [] -> []
  | (x, t) :: sb ->
     let sb' = make_subst sb in
     let t' = subst sb' t in
     if occurs x t' then
       sb'
     else
       let f (u, v) =
         u, subst [x, t'] v in
       (x, t') :: List.map f sb'

let collect_subst atoms =
  let f sb = function
    | P (_, _) -> sb
    | E (t, C x) -> (x, t) :: sb
    | _ -> assert false in
  make_subst @@ List.fold_left f [] atoms

let not_same_eq = function
  | P (_, _) -> true
  | E (F (_, []), _) -> true
  | E (x, y) -> not @@ eq x y
  | U _ -> true

let unflatten facts =
  let atoms = List.map transform facts in
  let sb = collect_subst atoms in
  let atoms = List.map (subst_atom sb) atoms in
  List.filter not_same_eq atoms
