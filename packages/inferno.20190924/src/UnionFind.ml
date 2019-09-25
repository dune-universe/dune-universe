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

(** This module implements a simple and efficient union/find algorithm.
    See Robert E. Tarjan, ``Efficiency of a Good But Not Linear Set
    Union Algorithm'', JACM 22(2), 1975. *)

(** The abstraction defined by this module is a set of points,
    partitioned into equivalence classes. With each equivalence class,
    a piece of information, of abstract type ['a], is associated; we
    call it a descriptor.

    A point is implemented as a cell, whose (mutable) contents consist
    of a single link to either information about the equivalence class,
    or another point. Thus, points form a graph, which must be acyclic,
    and whose connected components are the equivalence classes. In
    every equivalence class, exactly one point has no outgoing edge,
    and carries information about the class instead. It is the class's
    representative element.

    Information about a class consists of an integer weight (the number
    of elements in the class) and of the class's descriptor. *)
type 'a point =
    'a link ref

and 'a link =
  | Info of (* weight: *) int * (* descriptor: *) 'a
  | Link of 'a point

(** [fresh desc] creates a fresh point and returns it. It forms an
    equivalence class of its own, whose descriptor is [desc]. *)
let fresh desc =
  ref (Info (1, desc))

(** [repr point] returns the representative element of [point]'s
    equivalence class. It is found by starting at [point] and following
    the links. For efficiency, the function performs path compression
    at the same time. *)
let rec repr point =
  match !point with
  | Link point' ->
      let point'' = repr point' in
      if point'' != point' then

	(* [point''] is [point']'s representative element. Because we just
	   invoked [repr point'], [!point'] must be [Link point'']. We
	   write this value into [point], performing path compression.
           Note that we do not perform memory allocation. *)

	point := !point';
      point''
  | Info _ ->
      point

(** [find point] returns the descriptor associated with [point]'s
    equivalence class. *)
let rec find point =

  (* By not calling [repr] immediately, we optimize the common cases
     where the path starting at [point] has length 0 or 1, at the
     expense of the general case. *)

  match !point with
  | Info (_, desc) ->
      desc
  | Link point' ->
      match !point' with
      | Info (_, desc) ->
          desc
      | Link _ ->
          find (repr point)

(** [union f point1 point2] merges the equivalence classes associated
    with [point1] and [point2] into a single class. Then, (and only
    then,) it sets the descriptor of this class to the one produced by
    applying the function [f] to the descriptors of the two original
    classes. It has no effect if [point1] and [point2] are already in
    the same equivalence class.

    The fact that [point1] and [point2] do not originally belong to the
    same class guarantees that we do not create a cycle in the graph.

    The weights are used to determine whether [point1] should be made
    to point to [point2], or vice-versa. By making the representative
    of the smaller class point to that of the larger class, we
    guarantee that paths remain of logarithmic length (not accounting
    for path compression, which makes them yet smaller). *)
let union f point1 point2 =
  let point1 = repr point1
  and point2 = repr point2 in
  if point1 != point2 then
    match !point1, !point2 with
    | Info (weight1, desc1), Info (weight2, desc2) ->
        (* Be careful: the function [f] must not be invoked before the
           equivalence classes have been merged. *)
        if weight1 >= weight2 then begin
          point2 := Link point1;
          point1 := Info (weight1 + weight2, f desc1 desc2)
        end
        else begin
          point1 := Link point2;
          point2 := Info (weight1 + weight2, f desc1 desc2)
        end
    | Link _, _
    | _, Link _ ->
        assert false (* [repr] guarantees that [link] matches [Info _]. *)

(** [equivalent point1 point2] tells whether [point1] and [point2]
    belong to the same equivalence class. *)
let equivalent point1 point2 =
  repr point1 == repr point2

(** [is_representative] maps exactly one member of each equivalence class
    to [true]. *)
let is_representative point =
  match !point with
  | Link _ ->
      false
  | Info _ ->
      true
