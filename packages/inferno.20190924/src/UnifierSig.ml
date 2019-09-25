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

(* This file defines the input and output signatures of the functor
   [Unifier.Make]. *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* The structure of types is described to the unifier as follows. *)

(* BEGIN STRUCTURE *)
module type STRUCTURE = sig

  (* The type ['a structure] should be understood as a type of shallow (that
     is, depth 1) types whose leaves have type ['a]. *)
  type 'a structure

  (* The type ['a structure] should be a functor, i.e., it should support the
     standard [map] operation. *)
  val map: ('a -> 'b) -> 'a structure -> 'b structure

  (* We also require [iter] and [fold]. *)
  val iter: ('a -> unit) -> 'a structure -> unit
  val fold: ('a -> 'b -> 'b) -> 'a structure -> 'b -> 'b

  (* We also require [iter2], which fails if the head constructors differ. *)
  exception Iter2
  val iter2: ('a -> 'b -> unit) -> 'a structure -> 'b structure -> unit

(* END *)
end
(* END STRUCTURE *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

module type UNIFIER = sig

(* This is again the type of shallow types. *)

type 'a structure

(* The unifier maintains a graph whose vertices are known as variables. *)

type variable

(* Each variable carries three pieces of information, namely:

   - a unique identifier [id];

   - an optional [structure], which defines the constructor and
     the outgoing edges carried by this variable;

   - an integer [rank].

   The following read & write accessors are offered: *)

val            id: variable -> int

val     structure: variable -> variable structure option
val set_structure: variable -> variable structure option -> unit

val          rank: variable -> int
val      set_rank: variable -> int -> unit

(* [adjust_rank v k] is equivalent to [if k < rank v then set_rank v k] and
   equivalent to [set_rank v (min k (rank v))]. We offer this special-purpose
   accessor because this may help avoid some errors and gain some speed. *)

val   adjust_rank: variable -> int -> unit

(* -------------------------------------------------------------------------- *)

(* A new variable, with specified structure and rank, is obtained as follows.
   A fresh identifier is automatically picked. *)

val fresh: variable structure option -> int -> variable

(* -------------------------------------------------------------------------- *)

(* The main service offered by the unifier is the function [unify], which
   merges two variables [v1] and [v2].

   The unique identifier of the merged variable is arbitrarily chosen between
   the identifiers of [v1] and [v2].

   The structures carried by [v1] and [v2], if present, are recursively unified:
   that is, [unify] is recursively invoked for every pair of matching
   successors. If these structures exhibit incompatible constructors, then
   [Unify (v1, v2)] is raised, and the state of the unifier is unaffected.
   Unification may cause cycles in the graph to appear; this is explicitly
   supported. An occurs check is provided separately; see below. Note that
   the types [v1] and [v2] carried by the exception [Unify] can be cyclic!

   The rank of the merged variable is the minimum of the ranks of [v1] and
   [v2]. *)

exception Unify of variable * variable

val unify: variable -> variable -> unit

(* -------------------------------------------------------------------------- *)

(* Because every variable carries a unique identifier, it is easy to define
   a hash table whose keys are variables. Be careful, though: identifiers are
   stable only as long as no unification is performed. So, a hash table should
   not be used across a call to [unify]. *)

module VarMap : Hashtbl.S with type key = variable

(* -------------------------------------------------------------------------- *)

(* [equivalent v1 v2] tells whether [v1] and [v2] belong to the same equivalence
   class. *)

val equivalent: variable -> variable -> bool

(* [is_representative] maps exactly one member of each equivalence class to
   [true]. *)

val is_representative: variable -> bool

(* -------------------------------------------------------------------------- *)

(* [new_occurs_check is_young] initiates a new cycle detection phase. It
   produces a function [check] which can then be applied to a number of
   roots. If a cycle is detected, [Cycle] is raised. The occurs check is
   performed only over the young variables, i.e., those that satisfy the
   user-supplied predicate [is_young]. *)

exception Cycle of variable

val new_occurs_check: (variable -> bool) -> (variable -> unit)

(* -------------------------------------------------------------------------- *)

(* [new_acyclic_decoder leaf fold] initiates a new decoding phase. It produces a
   function [decode], which can then be applied to a number of roots. Decoding
   is a bottom-up computation of a value, and requires the graph to be acyclic
   (see [new_occurs_check] above).

   The functions [leaf] and [fold] are invoked (only) at a newly discovered
   variable; [leaf] is invoked if the variable has no structure, while [fold]
   is invoked if it has some structure. At a variable that has already been
   visited, the previously computed value is automatically returned.

   Decoding can be used, for instance, to transform the graph into a tree.
   Decoding runs in overall linear time and space. However, if the resulting
   tree is traversed in a naive way, an exponential blowup can take place! *)

val new_acyclic_decoder:
  (* leaf :      *) (    variable -> 'a) ->
  (* fold :      *) ('a structure -> 'a) ->
                    (    variable -> 'a)

(* -------------------------------------------------------------------------- *)

(* [new_cyclic_decoder] is analogous to [new_acyclic_decoder], but supports
   cyclic graphs. When a cycle is detected at a variable [x], instead of going
   further down, we stop and produce [leaf x]. Then, when this cycle is
   exited, also at [x], the function [mu] is used to combine [x] with the
   value that was obtained at the top of the cycle. The algorithm ensures that
   every cyclic use of a variable is dominated by a use of [mu]. The code is
   potentially exponentially inefficient. Use with caution! *)

val new_cyclic_decoder:
  (* leaf :      *) (      variable -> 'a) ->
  (* fold :      *) (  'a structure -> 'a) ->
  (* mu        : *) (variable -> 'a -> 'a) ->
                    (      variable -> 'a)

end

