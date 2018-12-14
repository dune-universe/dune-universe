(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Monadic traversal.

    [Traversable] provides signatures and functors for containers and
    data structures that can be 'traversed': mapped across with a
    monadic side-effect.  It resembles the Haskell Traversable
    typeclass, but with two differences:

    - We currently define traversability in terms of monads, not
      applicative functors (this might change in the future, but
      monads are generally more well-understood and well-supported in
      the OCaml/Core ecosystem);
    - as a result, the main 'traverse' function is called [map_m]. *)

open Base

(** {2 Signatures} *)

include module type of Traversable_intf
(** {{!Traversable_intf}Traversable_intf} contains the signatures for
    [Traversable]. *)

(** {2 Making containers}

    Monadic traversal is sufficient to define [fold], and, therefore,
    all of the key functionality for a Core-style container.  As such, we
    expose functors for building traversable Core-style containers. *)

module Make_container0 (I : Basic_container0)
  : S0_container with type t := I.t and type elt := I.Elt.t
(** [Make_container0] makes a {{!S0_container}S0_container} from a
    {{!Basic_container0}Basic_container0}. *)

module Make_container1 (I : Basic_container1)
  : S1_container with type 'a t := 'a I.t
(** [Make_container1] makes a {{!S1_container}S1_container} from a
    {{!Basic_container1}Basic_container1}. *)

(** {2 Helper functions} *)

(** [Helpers] contains utility functions for building traversals.

    Functions beginning [proc_variant] are useful for building
    traversal functions on top of Variantslib's [map] function.  The
    function [proc_field] is useful for building fold-map functions
    on top of Fieldslib's [fold] function. *)
module Helpers (M : Monad.S) : sig
  type 'a traversal = ('a -> 'a M.t)
  (** [traversal] is shorthand for a traversal function over [M]. *)

  val proc_variant0
    :  unit traversal
    -> 'cont Base.Variant.t
    -> 'cont M.t
  (** [proc_variant0 f variant] lifts a traversal [f] over a
      Variantslib nullary variant constructor [variant]. *)

  val proc_variant1
    :  'a traversal
    -> ('a -> 'cont) Base.Variant.t
    -> 'a
    -> 'cont M.t
  (** [proc_variant1 f variant a] lifts a traversal [f] over a
      Variantslib unary variant constructor [variant] with argument
      [a]. *)

  val proc_variant2
    : ('a * 'b) traversal
    -> ('a -> 'b -> 'cont) Base.Variant.t
    -> 'a
    -> 'b
    -> 'cont M.t
  (** [proc_variant2 f variant a b] lifts a traversal [f] over a
      Variantslib binary variant constructor [variant] with arguments
      [a] and [b]. *)

  val proc_variant3
    :  ('a * 'b * 'c) traversal
    -> ('a -> 'b -> 'c -> 'cont) Base.Variant.t
    -> 'a
    -> 'b
    -> 'c
    -> 'cont M.t
  (** [proc_variant3 f variant a b c] lifts a traversal [f] over a
      Variantslib ternary variant constructor [variant] with
      arguments [a], [b], and [c]. *)

  val proc_field
    :  'elt traversal
    -> 'cont M.t
    -> ([> `Set_and_create], 'cont, 'elt) Field.t_with_perm
    -> 'cont M.t
  (** [proc_field f state field container original] lifts a
      traversal [f] to a form comparible with Fieldslib's [fold]
      function. *)
end
