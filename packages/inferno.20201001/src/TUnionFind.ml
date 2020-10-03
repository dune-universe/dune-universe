(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

(* This module implements a transactional variant of the union-find
   algorithm. It uses transactional references instead of ordinary
   references, so that a series of operations performed within a
   transaction can be either committed or rolled back. *)

(* See [UnionFind] for comparison. The differences are:
   - we use [TRef.cell] instead of [ref];
   - [find] does not perform path compression, so as to avoid
     requiring a transaction parameter;
   - [union] requires a transaction parameter. *)

(* TEMPORARY offer a version of [find] that performs path compression
   and must be used outside a transaction? would be useful when we
   walk the unification graph. *)

type 'a point =
  'a link TRef.cell

and 'a link =
  | Info of (* weight: *) int * (* descriptor: *) 'a
  | Link of 'a point

let fresh desc =
  TRef.create (Info (1, desc))

(* This version of [repr] does not perform path compression. Thus, it can be
   used outside a transaction. *)

let rec repr point =
  match TRef.get point with
  | Link point' ->
      repr point'
  | Info _ ->
      point

let rec find point =
  match TRef.get point with
  | Info (_, desc) ->
      desc
  | Link point ->
      find (repr point)

let equivalent point1 point2 =
  repr point1 == repr point2

let is_representative point =
  match TRef.get point with
  | Link _ ->
      false
  | Info _ ->
      true

(* This version of [repr] performs path compression and must be used within a
   transaction. (If [TRef] offered a write operation that works both within
   and outside a transaction, we could use that.) *)

let rec repr t point =
  match TRef.get point with
  | Link point' ->
      let point'' = repr t point' in
      if point'' != point' then
	TRef.set t point (TRef.get point');
      point''
  | Info _ ->
      point

let union t f point1 point2 =
  let point1 = repr t point1
  and point2 = repr t point2 in
  if point1 != point2 then
    match TRef.get point1, TRef.get point2 with
    | Info (weight1, desc1), Info (weight2, desc2) ->
        if weight1 >= weight2 then begin
          TRef.set t point2 (Link point1);
          TRef.set t point1 (Info (weight1 + weight2, f desc1 desc2))
        end
        else begin
          TRef.set t point1 (Link point2);
          TRef.set t point2 (Info (weight1 + weight2, f desc1 desc2))
        end
    | _, Link _
    | Link _, _ ->
        assert false (* cannot happen *)
