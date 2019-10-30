(* Match a conjunction of atomic formulas with a structure *)

(* The matcher uses streams of subsititions to explore all possible
   matches. *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Prelude
open Sym
open Formula
open Structure

type env = (sym * sym) list     (* Map from variables to constants *)

(* Match a flattened term.  The case in which a flattened term is an
   equality between variables disappears because of previous
   substitutions. *)

(* The relation associated with a predicate or a function is
   represented as a trie.  The pattern is a list of terms each is
   either a variable or a constant. *)

let rec match_trie next pattern trie sb =
  match pattern, trie with
  | [], [] -> Cons (sb, next)           (* Substitution found *)
  | C c :: ps as xs, T (z, zs) :: ts -> (* Pattern is a constant *)
     let i = cmp c z in
     if i < 0 then              (* constant too small *)
       next ()
     else if i = 0 then         (* constant matches *)
       match_trie next ps zs sb
     else
       match_trie next xs ts sb
  | V v :: ps as xs, T (z, zs) :: ts -> (* Pattern is a variable *)
     let next () = match_trie next xs ts sb in
     match_trie next ps zs ((v, z) :: sb)
  | _ -> next ()

(* Match with a constant on the left. *)
let match_const next sb x y =
  match x, y with
  | C x, C y ->
     if same x y then
       Cons (sb, next)
     else
       next ()
  | C x, V y ->
     Cons ((y, x) :: sb, next)
  | _ -> assert false

let match_universe next sb trie = function
  | F _ -> assert false
  | x -> match_trie next [x] trie sb

(* Apply a substitution *)
let subst sb = function
  | V x as v ->
     (match List.assoc_opt x sb with
      | None -> v
      | Some y -> C y)
  | x -> x

(* Lookup relations associated with predicates and functions match
   with the resulting tries. *)
let match_an_atom next stc sb = function
  | P (s, xs) ->
     match_trie next (List.map (subst sb) xs) (lookup_pred stc s) sb
  | E (F (s, xs), x) ->
     let pat = List.map (subst sb) (x :: xs) in
     match_trie next pat (lookup_func stc s) sb
  | E (x, y) ->
     match_const next sb (subst sb x) (subst sb y)
  | U x ->
     match_universe next sb (lookup_univ stc) (subst sb x)

(* Maps a stream of substitions to another by extending each
   substitution via matching using the atom as a pattern *)
let rec match_atom stc atom = function
  | Nil -> Nil
  | Cons (sb, k) ->
     let next () = match_atom stc atom (k ()) in
     match_an_atom next stc sb atom

let rec match_loop stc stream = function
  | [] -> stream
  | atom :: atoms ->
     match_loop stc (match_atom stc atom stream) atoms

(* Match a conjunction using streams of substitutions. *)
let match_conj stc (And conj) =
  let mt_sb = Cons ([], fun () -> Nil) in
  match_loop stc mt_sb conj
