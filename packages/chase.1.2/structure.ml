(* Structures *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Sym
open Formula
open Fact
open Cong_close

(* Abstractly, a trie is a set of lists of symbols of the same
   length. *)

(* Structure [stc] contains two maps.  [stc.preds] maps a predicate
   symbol to a trie.  [stc.funcs] maps a function symbol to a trie.
   For a predicate symbol, the length of the lists in the trie is the
   same as the arity of the predicate symbol.  For a function symbol,
   the length of the lists in the trie is one more than the arity of
   the function symbol.  If

   f(s, xs) = x

   is in the structure, than [x :: xs] is in the trie associated with
   function symbol [f]. *)

type structure = {
    preds : (trie list) SymHashtbl.t;    (* Facts about a predicate *)
    funcs : (trie list) SymHashtbl.t;    (* Facts about a function *)
    mutable univ : trie list option      (* The domain of discourse *)
  }

(* Lookup a predicate symbol *)
let lookup_pred stc x = find_trie stc.preds x

(* Lookup a function symbol *)
let lookup_func stc x = find_trie stc.funcs x

let univ_extent set tbl =
  let f _ trie set =
    fold_trie (fun s e -> add e s) set trie in
  SymHashtbl.fold f tbl set

(* Compute the universe of this structure. *)
let univ stc =
  let u = univ_extent [] stc.preds in
  let u = univ_extent u stc.funcs in
  let f x = T (x, []) in        (* Make unary extent *)
  List.map f u

(* Lookup the universe as unary predicate *)
let lookup_univ stc =
  match stc.univ with
  | None ->
     let u = univ stc in
     stc.univ <- Some u;
     u
  | Some u -> u

(* Compute the sum of the number of elements in all tries *)
let size stc =
  let f _ trie n = n + count trie in
  SymHashtbl.fold f stc.preds @@ SymHashtbl.fold f stc.funcs 0

(* Extract the facts associated with a structure.  The resulting list
   is sorted. *)
let facts stc =
  let pred sym trie set =
    let f set xs =
      FactSet.add (Q (sym, xs)) set in
    List.fold_left f set @@ to_list trie in
  let func sym trie set =
    let f set = function
      | [] -> assert false
      | x :: xs ->
         FactSet.add (G (sym, xs, x)) set in
    List.fold_left f set @@ to_list trie in
  FactSet.elements
    (SymHashtbl.fold pred stc.preds
       (SymHashtbl.fold func stc.funcs FactSet.empty))

(* Make an empty structure *)
let mt_struct () =
  let n = 8 in
  let preds = SymHashtbl.create n in
  let funcs = SymHashtbl.create n in
  { preds; funcs; univ = None }

(* Augment a structure and put it into normal form *)

(* A structure is in normal form iff equality is closed under
   conquence and each constant is the canonical representative of an
   equivalence class induced by congruence closure. *)

(* Make a constant term *)
let const_term x =
  mk_term x []

(* Find equivalence class for a constant. *)
let insert tbl x =
  intern tbl (const_term x)

(* Put x and y into the same equivalence class. *)
let equate tbl x y =
  let c = insert tbl x in
  let d = insert tbl y in
  merge c d

(* Extract a constant from a term than is know to be a constant. *)
let term_const = function
  | C x -> x
  | V _ -> assert false
  | F (_, _) -> assert false

(* Load a function into equivalence classes. *)
let load_func tbl s x xs =
  let y = insert tbl x in
  let ys = List.map (insert tbl) xs in
  let lhs = intern tbl (mk_term s (List.map const_term xs)) in
  merge lhs y;              (* Equate here *)
  y :: ys                   (* Return equivalence classes as a list *)

(* Add atoms into equivalence classes and return two lists.  The first
   list is an association list that maps a predicate symbol to a list
   of equivalence classes.  The second is for function symbols.  *)
let add_atoms tbl atoms =
  let f (ps, fs) = function              (* Add each atom *)
    | P (s, xs) ->
       let g x = insert tbl @@ term_const x in
       (s, List.map g xs) :: ps, fs (* Put preds on first list *)
    (* Found a constant equated to a constant *)
    | E (C x, y) ->
       equate tbl x @@ term_const y;
       ps, fs
    | E (F (s, xs), x) ->
       let ys = List.map term_const xs in (* Put funcs on second *)
       ps, (s, load_func tbl s (term_const x) ys) :: fs (* list *)
    | _ -> assert false in
  List.fold_left f ([], []) atoms

(* Load functions into the equivalence classes.  Produces an
   association list with function symbols as keys, and lists of lists
   of equivalence classes as the values. *)
let load tbl funcs =
  let f s trie alist =          (* For each function entry *)
    let g = function            (* For each member of trie *)
      | [] -> assert false
      | x :: xs ->              (* x is value of function *)
         load_func tbl s x xs in
    (s, List.map g @@ to_list trie) :: alist in
  SymHashtbl.fold f funcs []

(* Get the canonical representative of an equivalence class. *)
let repr c =
  let t = term c in
  if term_args t == [] then
    term_func t
  else
    assert false                (* This should never happen *)

(* Make function table entry *)
let mk_func funcs (s, eqvs) =
  let f trie xs =
    Sym.insert trie (List.map repr xs) in
  let trie = List.fold_left f [] eqvs in
  SymHashtbl.add funcs s trie

(* Make predicate table entry *)
let mk_pred tbl preds s trie =
  let f trie xs =
    let g x = repr (insert tbl x) in
    Sym.insert trie (List.map g xs) in
  let trie = List.fold_left f [] (to_list trie) in
  SymHashtbl.add preds s trie

let add_item tbl (s, xs) =
  insert_trie tbl s @@ List.map repr xs

let augment_struct stc atoms =
  let n = List.length atoms in
  let tbl = mk_tbl () in
  let ps, fs = add_atoms tbl atoms in (* Add atoms *)
  let afuncs = load tbl stc.funcs in  (* Make alist for functions *)
  (* Create new funcs table *)
  let nfuncs = SymHashtbl.create @@ n + SymHashtbl.length stc.funcs in
  List.iter (mk_func nfuncs) afuncs;
  List.iter (add_item nfuncs) fs;
  (* Create new precs table *)
  let npreds = SymHashtbl.create @@ n + SymHashtbl.length stc.preds in
  SymHashtbl.iter (mk_pred tbl npreds) stc.preds;
  List.iter (add_item npreds) ps;
  { preds = npreds; funcs = nfuncs; univ = None }
