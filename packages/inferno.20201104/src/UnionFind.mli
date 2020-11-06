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
    call it a descriptor. *)
type 'a point

(** [fresh desc] creates a fresh point and returns it. It forms an
    equivalence class of its own, whose descriptor is [desc]. *)
val fresh: 'a -> 'a point

(** [find point] returns the descriptor associated with [point]'s
    equivalence class. *)
val find: 'a point -> 'a

(** [union f point1 point2] merges the equivalence classes associated
    with [point1] and [point2] into a single class. Then, (and only
    then,) it sets the descriptor of this class to the one produced by
    applying the function [f] to the descriptors of the two original
    classes. It has no effect if [point1] and [point2] are already in
    the same equivalence class. *)
val union: ('a -> 'a -> 'a) -> 'a point -> 'a point -> unit

(** [equivalent point1 point2] tells whether [point1] and [point2]
    belong to the same equivalence class. *)
val equivalent: 'a point -> 'a point -> bool

(** [is_representative] maps exactly one member of each equivalence class
    to [true]. *)
val is_representative: 'a point -> bool

