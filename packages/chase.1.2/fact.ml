(* Facts and sets of facts *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Sym

type fact =
  (* Predicate symbol applied to constants *)
  | Q of sym * sym list
  (* Function symbol applied to constants gives constant *)
  | G of sym * sym list * sym

(* Comparisons *)

(* A total ordering for a list of symbols *)
let rec compr_list xs ys =
  match xs, ys with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x :: xs, y :: ys ->
     match cmp x y with
     | 0 -> compr_list xs ys
     | c -> c

(* A total ordering for facts *)
let compr x y =
  match x, y with
  | Q (s, xs), Q (t, ys) ->
     (match cmp s t with
      | 0 -> compr_list xs ys
      | c -> c)
  | Q _, G _ -> -1
  | G _, Q _ -> 1
  | G (s, xs, a), G (t, ys, b) ->
     match cmp s t with
     | 0 ->
       (match compr_list xs ys with
        | 0 -> cmp a b
        | c -> c)
     | c -> c

module OrderedFact =
  struct
    type t = fact
    let compare = compr
  end

module FactSet = Set.Make(OrderedFact)
