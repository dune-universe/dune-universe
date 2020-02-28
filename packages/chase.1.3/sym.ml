(* Symbols *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

(* A symbol is a pair consisting of an integer and a string.
   Symbols are the same when their integers agree.  The string is
   used for printing. *)

type sym = Sym of int * string

let gen = ref 0

(* Generate a fresh symbol.  This is the only way to create a
   symbol. *)
let mk_sym s =
  let sym = Sym (!gen, s) in
  incr gen;
  sym

let int_of_sym (Sym (i, _)) = i

let string_of_sym (Sym (_, s)) = s

(* The implementation ensures that when int_of_sym x = int_of_sym y
   then string_of_sym x = string_of_sym y. *)

let clone (Sym (_, s)) = mk_sym s

(* Comparisons *)

let same (Sym (x, _)) (Sym (y, _)) = x = y

let cmp (Sym (x, _)) (Sym (y, _)) = compare x y

(* Association lists *)

let rec assoc_opt x = function
  | [] -> None
  | (y, z) :: _ when same x y -> Some z
  | _ :: xs -> assoc_opt x xs

module SymHash =
  struct
    type t = sym
    let equal x y = same x y
    let hash x = int_of_sym x land max_int
  end

module SymHashtbl = Hashtbl.Make(SymHash)

(* Contexts *)

let is_digit c =
  c >= '0' && c <= '9'

(* Are the characters starting at i digits in string s? *)
let is_digits s i =
  let n = String.length s in
  let rec loop i =
    i >= n ||
      is_digit s.[i] && loop (i + 1) in
  loop i

(* Print a symbol while adding a suffix in case of name collision. *)

let suffix_char = '_'
let suffix_string = String.make 1 suffix_char

(* Compute the root name of a string.  The root_name function removes
   the suffix characters and trailing digits from a string if they
   exists. *)
let rec root_name s =
  match String.rindex_opt s suffix_char with
  | None -> s
  | Some i ->
     if i > 0 && i + 1 < String.length s && is_digits s (i + 1) then
       root_name @@ String.sub s 0 i (* Must recurse or pname breaks *)
     else
       s

module IntKey =
  struct
    type t = int
    let compare = compare
  end

module IntMap = Map.Make(IntKey)

(* A map from the integer of a symbol to its print string *)
type namings = int * string IntMap.t

(* A map from root names to namings *)
type ctx = (string, namings) Hashtbl.t

let mk_ctx () = Hashtbl.create 64

(* Produce the print name for x in context ctx. *)
let pname ctx x =
  let root = root_name @@ string_of_sym x in
  match Hashtbl.find_opt ctx root with
  | None ->
     Hashtbl.add ctx root (0, IntMap.add (int_of_sym x) root IntMap.empty);
     root
  | Some (i, im) ->             (* Root name found *)
     match IntMap.find_opt (int_of_sym x) im with
     | Some s -> s              (* Found entry *)
     | None ->                  (* Not found - add new entry *)
        let str = root ^ suffix_string ^ string_of_int i in
        Hashtbl.replace ctx root (i + 1, IntMap.add (int_of_sym x) str im);
        str

(* Set of symbols as ordered lists *)

let rec mem x = function
  | [] -> false
  | y :: ys ->
     let c = cmp x y in
     c = 0 || c > 0 && mem x ys

let rec add x = function
  | [] -> [x]
  | y :: ys as z ->
     let c = cmp x y in
     if c = 0 then
       z
     else
       if c < 0 then
         x :: z
       else
         y :: add x ys

let rec remove x = function
  | [] -> []
  | y :: ys as z ->
     let c = cmp x y in
     if c = 0 then
       ys
     else
       if c < 0 then
         z
       else
         y :: remove x ys

let diff xs ys =
  let f x = not (mem x ys) in
  List.filter f xs

(* Tries of symbols *)

type trie =                     (* Trie node *)
  | T of sym * trie list

let rec create = function
  | [] -> []
  | x :: xs -> [T (x, create xs)]

let rec insert trie = function
  | [] -> trie
  | x :: xs as y ->
     match trie with
     | [] -> create y
     | T (z, zs) as t :: ts ->
        let i = cmp x z in
        if i < 0 then
          create y @ t :: ts
        else if i = 0 then
          T (z, insert zs xs) :: ts
        else
          t :: insert ts y

(*
let rec to_list trie =
  let f = function
    | T (x, []) -> [[x]]
    | T (x, xs) ->
      List.map (fun ys -> x :: ys) (to_list xs) in
  List.concat @@ List.map f trie
 *)

(* A faster to_list (I hope) *)
let rec to_list = function
  | [] -> []
  | T (z, []) :: xs ->
     [z] :: to_list xs
  | T (z, zs) :: xs ->
     let rec loop = function
       | [] -> to_list xs
       | y :: ys ->
          (z :: y) :: loop ys in
     loop @@ to_list zs

let count trie =
  let rec f n = function
    | T (_, []) -> n + 1
    | T (_, xs) ->
       List.fold_left f n xs in
  List.fold_left f 0 trie

let rec fold_trie f a xs =
  let g a = function
    | T (x, xs) ->
       fold_trie f (f a x) xs in
  List.fold_left g a xs

(* Tables of tries *)

let find_trie tbl key =
  match SymHashtbl.find_opt tbl key with
  | None -> []
  | Some trie -> trie

let insert_trie tbl key item =
  match SymHashtbl.find_opt tbl key with
  | None ->
     SymHashtbl.add tbl key (insert [] item)
  | Some trie ->
     SymHashtbl.replace tbl key (insert trie item)

(* For debugging *)

let print_sym f (Sym (n, s)) =
  Format.pp_print_string f s;
  Format.pp_print_string f "_";
  Format.pp_print_int f n
