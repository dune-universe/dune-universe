(***************************************************************************)
(*                                                                         *)
(*                                 UnionFind                               *)
(*                                                                         *)
(*                       François Pottier, Inria Paris                     *)
(*                                                                         *)
(*  Copyright Inria. All rights reserved. This file is distributed under   *)
(*  the terms of the GNU Library General Public License version 2, with a  *)
(*  special exception on linking, as described in the file LICENSE.        *)
(***************************************************************************)

(* This module offers a union-find data structure based on disjoint set forests,
   with path compression and linking by rank. *)

(* -------------------------------------------------------------------------- *)

(* The rank of a vertex is the maximum length, in edges, of an uncompressed path
   that leads to this vertex. In other words, the rank of [x] is the height of
   the tree rooted at [x] that would exist if we did not perform path
   compression. *)

type rank =
  int

(* The content of a vertex is a pointer to a parent vertex (if the vertex has a
   parent) or a pair of a rank and a user value (if the vertex has no parent,
   and is thus the representative vertex for this equivalence class). *)

type 'a content =
| Link of 'a elem
| Root of rank * 'a

(* The type ['a elem] represents a vertex in the union-find data structure. *)

and 'a elem =
  'a content ref

(* -------------------------------------------------------------------------- *)

(* [make v] creates a new root of rank zero. *)

let make (v : 'a) : 'a elem =
  ref (Root (0, v))

(* -------------------------------------------------------------------------- *)

(* [find x] finds the representative vertex of the equivalence class of [x].
   It does so by following the path from [x] to the root. Path compression is
   performed (on the way back) by making every vertex along the path a
   direct child of the representative vertex. No rank is altered. *)

let rec find (x : 'a elem) : 'a elem =
  match !x with
  | Root (_, _) ->
      x
  | Link y ->
      let z = find y in
      if z != y then
        x := Link z;
      z

(* -------------------------------------------------------------------------- *)

(* [eq x y] determines whether the vertices [x] and [y] belong in the
   same equivalence class. It does so via two calls to [find] and a
   physical equality test. As a fast path, we first test whether [x]
   and [y] are physically equal. *)

let eq (x : 'a elem) (y : 'a elem) : bool =
  x == y || find x == find y

(* -------------------------------------------------------------------------- *)

(* [get x] returns the value stored at [x]'s representative vertex. *)

let get (x : 'a elem) : 'a =
  let x = find x in
  match !x with
  | Root (_, v) ->
      v
  | Link _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* [set x] updates the value stored at [x]'s representative vertex. *)

let set (x : 'a elem) (v : 'a) : unit =
  let x = find x in
  match !x with
  | Root (r, _) ->
      x := Root (r, v)
  | Link _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* [link x y] requires the vertices [x] and [y] to be roots. If they are
   the same vertex, it does nothing. Otherwise, it merges their equivalence
   classes by installing a link from one vertex to the other. *)

(* Linking is by rank: the smaller-ranked vertex is made to point to the
   larger. If the two vertices have the same rank, then an arbitrary choice
   is made, and the rank of the new root is incremented by one. *)

let link (x : 'a elem) (y : 'a elem) : 'a elem =
  if x == y then x else
    match !x, !y with
    | Root (rx, vx), Root (ry, _) ->
        if rx < ry then begin
          x := Link y; y
        end else if rx > ry then begin
          y := Link x; x
        end else begin
          y := Link x; x := Root (rx+1, vx); x
        end
    | Root _, Link _
    | Link _, Root _
    | Link _, Link _ ->
        assert false

(* -------------------------------------------------------------------------- *)

(* [union x y] is identical to [link s x y], except it does not require
   [x] and [y] to be roots. *)

let union (x : 'a elem) (y : 'a elem) : 'a elem =
  link (find x) (find y)
