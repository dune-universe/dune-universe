(* Congruenc closure *)

(* This implentation uses a fast find implementation of union-find.
   It chooses the symbol with the least number as the class'
   canonical representative whenever possible.  This is an easy
   modification when using fast-find, and is the reason for selecting
   this algorithm. *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Sym

(* Terms are a function symbol and an argument list.
   The argument list is empty for constants. *)

type term = F of sym * term list

let mk_term func args =
  F (func, args)

let term_func (F (func, _)) = func

let term_args (F (_, args)) = args

type node = {
    func: sym;                (* function symbol *)
    args: equiv list;         (* arguments *)
    mutable pred: pred list } (* predecessors: each use this in args *)

and pred = equiv * node

and equiv = {
    value: node;
    mutable root: root;     (* Class root *)
    mutable next: equiv }   (* Field used to create a circular list *)

and root = {
    repr: equiv;                (* The class representative *)
    mutable size: int }         (* Number of elements in class *)

(* Nodes of the smallest size are favored to be the class representative. *)
let size nd =
  if nd.args = [] then
    int_of_sym nd.func
  else
    max_int

(* Create a fresh equivalence class and its root *)
let create x =
  let rec c = {
      value = x;
      root = root;
      next = c;
    }
  and root = {
      repr = c;
      size = size x;
    } in
  c

(* Get the representative node of an equivalence class *)
let get c =
  c.root.repr.value

let same_class c1 c2 =
  c1.root == c2.root            (* Note physical equality used *)

(* Set root of class c to r *)
let set_root r c =
  let rec loop d =
    d.root <- r;
    if not (d.next == c) then
      loop d.next in
  loop c

(* Let c1 --> A --> B    where A is the class representative of c1.
              ^     |
              +-----+

   Let c2 --> X --> Y --> Z    where Z is the class representative.
              ^           |
              +-----------+

   After merge c1 c2

   c2 --> X --> A --> B --> Y --> Z
          ^                       |
          +-----------------------+
 *)

(* Merge c1 into c2 *)
let merge c1 c2 =
  set_root c2.root c1;
  let tmp = c2.next in
  c2.next <- c1.next;
  c1.next <- tmp

(* Join to classes into one *)
let union c1 c2 =
  if same_class c1 c2 then
    ()
  else
    if c1.root.size > c2.root.size then
      merge c1 c2
    else
      merge c2 c1

(* Table used to intern terms.  It's just an association list. *)

type key = sym * equiv list   (* function sym and arg equiv classes *)

type tbl = {
    mutable alist : (key * equiv) list;
  }

(* Are lists of classes the same? *)
let rec same_classes cs cs' =
  match cs, cs' with
  | [], [] -> true
  | c :: cs, c' :: cs' ->
     same_class c c' && same_classes cs cs'
  | _ -> false

(* Lookup equivalence class for given function and its arguments. *)
let rec lookup func args = function
  | [] -> None
  | ((f, xs), equiv) :: _ when same func f && same_classes args xs ->
     Some equiv
  | _ :: tbl -> lookup func args tbl

(* Create an empty intern table *)
let mk_tbl () = {alist = []}

(* Add element to list when not present *)
let add e l =
  if List.memq e l then         (* Use physical equality *)
    l
  else
    e :: l

(* Append lists while eliminating duplicates *)
let join l1 l2 =
  List.fold_right add l1 l2

(* Add a term to the equivalence class relation *)
let rec intern tbl (F (name, ts)) =
  let cs = List.map (intern tbl) ts in
  match lookup name cs tbl.alist with
  | Some c -> c
  | None ->
     let nd = {func=name; args=cs; pred=[]} in
     let c = create nd in
     let f c' =                 (* Add (c, nd) to predecessor list *)
       let n = get c' in
       n.pred <- add (c, nd) n.pred in
     List.iter f cs;
     tbl.alist <- ((name, cs), c) :: tbl.alist;
     c

(* Returns the representative term associated with an equivalence
   class c *)
let rec term c =
  let nd =  get c in
  mk_term nd.func (List.map term nd.args)

let congruent p p' =
  p.func = p'.func && same_classes p.args p'.args

(* Join two equivalence classes and update the predecessor list of the
   representative. *)
let union p p' =
  let d = (get p).pred in
  let d' = (get p').pred in
  union p p';    (* <- This union is defined earlier in this module *)
  (get p).pred <- join d d'

(* Merge two equivance classes using the Nelson and Oppen algorithm. *)
let rec merge p p' =
  if same_class p p' then
    ()
  else
    let d = (get p).pred in
    let d' = (get p').pred in
    union p p';
    let f (c, d) (c', d') =
      if not (same_class c c') && congruent d d' then
        merge c c' in
    let g p = List.iter (f p) d' in
    List.iter g d
