(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** Signatures and functors for containers and data structures that can be
    mapped across with a monadic side-effect.

    Modules based on this pattern resembles the Haskell Traversable
    typeclass, but with two differences:

    - We currently define traversability in terms of monads, not applicative
      functors (this might change in the future, but monads are generally
      more well-understood and well-supported in the OCaml/Core ecosystem);
    - as a result, the main 'traverse' function is called [map_m]. *)

open Base

(** {2 Signatures}

    For input and output module signatures for this module's functors, see
    {!Traversable_types}. *)

(** {2 Making traversable containers}

    Monadic traversal is sufficient to define [fold], and, therefore, all of
    the key functionality for a Base-style container. As such, our signatures
    and functors subsume those for building containers. *)

(** {3 New containers} *)

(** [Make] makes an {{!Traversable_types.S0} S0} from a
    {{!Traversable_types.Basic0} Basic0}. *)
module Make0 (I : Traversable_types.Basic0) :
  Traversable_types.S0 with module Elt = I.Elt and type t = I.t

(** [Make] makes an {{!Traversable_types.S1} S1} from a
    {{!Traversable_types.Basic1} Basic1}. *)
module Make1 (I : Traversable_types.Basic1) :
  Traversable_types.S1 with type 'a t = 'a I.t

(** {3 Extending existing containers with monadic traversals} *)

(** [Make0_container] makes an {{!Traversable_types.S0} S0} from a
    {{!Traversable_types.Basic0_container} Basic0_container}. *)
module Make0_container (I : Traversable_types.Basic0_container) :
  Traversable_types.S0 with module Elt = I.Elt and type t = I.t

(** [Make1_container] makes an {{!Traversable_types.S1} S1} from a
    {{!Traversable_types.Basic_container1} Basic1_container}. *)
module Make1_container (I : Traversable_types.Basic1_container) :
  Traversable_types.S1 with type 'a t = 'a I.t

(** {2 Combining and modifying traversable containers} *)

(** {3 Chaining} *)

(** [Chain0] chains two {{!Traversable_types.S0} S0} instances together,
    traversing each element of the outer instance with the inner instance. *)
module Chain0
    (Outer : Traversable_types.S0)
    (Inner : Traversable_types.S0 with type t := Outer.Elt.t) :
  Traversable_types.S0 with module Elt = Inner.Elt and type t = Outer.t

(** {3 Fixing the element type} *)

(** [Fix_elt (I) (Elt)] demotes an {{!Traversable_types.S1} S1} [S] to an
    {{!Traversable_types.S0} S0} by fixing the element type to that mentioned
    in [Elt]. *)
module Fix_elt (I : Traversable_types.S1) (Elt : Equal.S) :
  Traversable_types.S0 with module Elt = Elt and type t = Elt.t I.t

(** {2 Helper functions and functors} *)

(** [Const] constructs a {{!Traversable_types.S0} S0} over container [T] and
    arbitrary element type [Elt] that, when traversed, makes no calls to the
    supplied function and returns the container unchanged.

    {[
      module K = Const (String) (String)
      (* This would output "foo". *)
      K.map "foo" ~f:String.uppercase
    ]} *)
module Const (T : T) (Elt : Equal.S) :
  Traversable_types.S0 with module Elt = Elt and type t = T.t
