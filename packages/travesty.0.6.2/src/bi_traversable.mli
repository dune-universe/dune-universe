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
    mapped across in two different ways with a monadic side-effect.

    [Bi_traversable] is to {{!Bi_mappable} Bi_mappable} what {{!Traversable}
    Traversable} is to {{!Mappable} Mappable}. It approximates the concept
    with the same name that appears in various Haskell bifunctor libraries. *)

open Base

(** {2 Signatures}

    For input and output signatures for this module's functors, see
    {{!Bi_traversable_types} Bi_traversable_types}. *)

(** {2 Making full bi-traversable type modules}

    These functors build full implementations of bi-traversability given the
    basic minimal definitions above. *)

(** [Make2] implements {{!Bi_traversable_types.S2} S2} for an arity-2
    bi-traversable container. *)
module Make2 (I : Bi_traversable_types.Basic2) :
  Bi_traversable_types.S2 with type ('l, 'r) t = ('l, 'r) I.t

(** [Make1_left] implements {{!Bi_traversable_types.S1_left} S1_left} for an
    arity-1 bi-traversable container with floating left type. *)
module Make1_left (I : Bi_traversable_types.Basic1_left) :
  Bi_traversable_types.S1_left
    with type 'l t = 'l I.t
     and type right = I.right

(** [Make1_right] implements {{!Bi_traversable_types.S1_right} S1_right} for
    an arity-1 bi-traversable container with floating right type. *)
module Make1_right (I : Bi_traversable_types.Basic1_right) :
  Bi_traversable_types.S1_right
    with type 'r t = 'r I.t
     and type left = I.left

(** [Make0] implements {{!Bi_traversable_types.S0} S0} for an arity-0
    bi-traversable container. *)
module Make0 (I : Bi_traversable_types.Basic0) :
  Bi_traversable_types.S0
    with type t = I.t
     and type left = I.left
     and type right = I.right

(** {2:fix Fixing types}

    We can convert arity-2 modules to arity-1 modules, and arity-1 modules to
    arity-0 modules, by fixing types. The various [FixX_Y] functors achieve
    this. *)

(** {3 Arity-2} *)

(** [Fix2_left (I) (Left)] fixes the left type of [I] to [Left], making it an
    {{!Bi_traversable_types.S1_right} S1_right}. *)
module Fix2_left (I : Bi_traversable_types.Basic2) (Left : T) :
  Bi_traversable_types.S1_right
    with type 'r t = (Left.t, 'r) I.t
     and type left = Left.t

(** [Fix2_right (S) (Left)] fixes the right type of [S] to [Right], making it
    an {{!Bi_traversable_types.S1_left} S1_left}. *)
module Fix2_right (I : Bi_traversable_types.Basic2) (Right : T) :
  Bi_traversable_types.S1_left
    with type 'l t = ('l, Right.t) I.t
     and type right = Right.t

(** [Fix2_both (S) (Left) (Right)] fixes the types of [S] to [Left] and
    [Right], making it an {{!Bi_traversable_types.S0} S0}. *)
module Fix2_both (I : Bi_traversable_types.Basic2) (Left : T) (Right : T) :
  Bi_traversable_types.S0
    with type t = (Left.t, Right.t) I.t
     and type left = Left.t
     and type right = Right.t

(** {3 Arity-1} *)

(** [Fix1_left (S) (Left)] fixes the floating left type of [S] to [Left],
    making it an {{!Bi_traversable_types.S0} S0}. *)
module Fix1_left (I : Bi_traversable_types.Basic1_left) (Left : T) :
  Bi_traversable_types.S0
    with type t = Left.t I.t
     and type left = Left.t
     and type right = I.right

(** [Fix1_right (I) (Right)] fixes the floating right type of [S] to [Right],
    making it an {{!Bi_traversable_types.S0} S0}. *)
module Fix1_right (I : Bi_traversable_types.Basic1_right) (Right : T) :
  Bi_traversable_types.S0
    with type t = Right.t I.t
     and type left = I.left
     and type right = Right.t

(** {2 Converting bi-traversable modules to traversable modules}

    By ignoring values of either the left or the right type, we can derive
    {{!Traversable} traversable} modules from bi-traversable ones. Since the
    various [S] {i n} signatures contain functions for doing this on an
    ad-hoc basis, the functors below are mainly for use when one needs actual
    {{!Traversable} Traversable} instances.

    The various caveats that apply to fixing {{!Bi_mappable} bi-mappable}
    types apply. Note also that the various [Traverse0] functors are more
    restrictive than their arity-1 and arity-2 counterparts. *)

(** {3 Arity-1} *)

(** Traversing over the left type of an arity-1 bi-traversable container with
    a floating left type. *)
module Traverse1_left (S : Bi_traversable_types.S1_left) :
  Traversable_types.S1 with type 'l t = 'l S.t

(** Traversing over the right type of an arity-1 bi-traversable container
    with a floating right type. *)
module Traverse1_right (S : Bi_traversable_types.S1_right) :
  Traversable_types.S1 with type 'r t = 'r S.t

(** {3 Arity-0}

    Since arity-0 Base-style containers require their element to implement
    equality, the same restriction applies to arity-0 {{!Traversable}
    traversables}. This means that the arity-0 functors need to carry an
    extra parameter that witnesses this equality.

    As {{!Mappable} mappables} don't have this restriction, if one requires
    only non-monadic mappable functionality down one side of an arity 0
    bi-traversable, one can use the [Map0] functors in {{!Bi_mappable}
    Bi_mappable} with an {{!Bi_mappable_types.S0} S0}. *)

(** Traversing over the left type of an arity-0 bi-traversable container,
    which must have equality as witnessed by an [Equal.S] module. *)
module Traverse0_left
    (L : Equal.S)
    (S : Bi_traversable_types.S0 with type left := L.t) :
  Traversable_types.S0 with type t = S.t and module Elt = L

(** Traversing over the right type. of an arity-0 bi-traversable container,
    which must have equality as witnessed by an [Equal.S] module. *)
module Traverse0_right
    (R : Equal.S)
    (S : Bi_traversable_types.S0 with type right := R.t) :
  Traversable_types.S0 with type t = S.t and module Elt = R

(** {2 Chaining containers} *)

(** {3 Chaining a traversable on the outside of a bi-traversable}

    These functors let us compose an inner bi-traversable container with an
    outer traversable container, producing a bi-traversable.

    For example, we can make {{!Travesty_base_exts.Alist} associative lists}
    bi-traversable by composing a bi-traversable over
    {{!Travesty_base_exts.Tuple2} pairs} [(a * b)] with a traversable over
    {{!Travesty_base_exts.List} lists}. *)

(** [Chain_Bi2_Traverse1 (Bi) (Trav)] composes a bi-traversal [Bi] on an
    inner arity-2 container over a traversal [Trav] over an outer arity-1
    container. *)
module Chain_Bi2_Traverse1
    (Bi : Bi_traversable_types.S2)
    (Trav : Traversable_types.S1) :
  Bi_traversable_types.S2 with type ('l, 'r) t = ('l, 'r) Bi.t Trav.t

(** [Chain_Bi1_left_Traverse1 (Bi) (Trav)] composes a bi-traversal [Bi] on an
    inner arity-1 container with floating left type over a traversal [Trav]
    over an outer arity-1 container. *)
module Chain_Bi1_left_Traverse1
    (Bi : Bi_traversable_types.S1_left)
    (Trav : Traversable_types.S1) :
  Bi_traversable_types.S1_left
    with type 'l t = 'l Bi.t Trav.t
     and type right = Bi.right

(** [Chain_Bi1_right_Traverse1 (Bi) (Trav)] composes a bi-traversal [Bi] on
    an inner arity-1 container with floating right type over a traversal
    [Trav] over an outer arity-1 container. *)
module Chain_Bi1_right_Traverse1
    (Bi : Bi_traversable_types.S1_right)
    (Trav : Traversable_types.S1) :
  Bi_traversable_types.S1_right
    with type 'r t = 'r Bi.t Trav.t
     and type left = Bi.left

(** [Chain_Bi0_Traverse1 (Bi) (Trav)] composes a bi-traversal [Bi] on an
    inner arity-0 container over a traversal [Trav] over an outer arity-1
    container. *)
module Chain_Bi0_Traverse1
    (Bi : Bi_traversable_types.S0)
    (Trav : Traversable_types.S1) :
  Bi_traversable_types.S0
    with type t = Bi.t Trav.t
     and type left = Bi.left
     and type right = Bi.right

(** {3 Chaining traversables on the inside of a bi-traversable}

    These functors let us compose one or two inner traversable containers
    with an outer bi-traversable container, producing a bi-traversable. *)

(** [Chain_Traverse1_Bi2 (LTrav) (RTrav) (Bi)] composes an inner arity-1
    traversal [LTrav] on the left, and another such traversal [RTrav] on the
    right, of an arity-2 bi-traversal [Bi]. *)
module Chain_Traverse1_Bi2
    (LTrav : Traversable_types.S1)
    (RTrav : Traversable_types.S1)
    (Bi : Bi_traversable_types.Basic2) :
  Bi_traversable_types.S2
    with type ('l, 'r) t = ('l LTrav.t, 'r RTrav.t) Bi.t

(** [Chain_Traverse1_Bi1_left (LTrav) (Bi)] composes an inner arity-1
    traversal [LTrav] on the left of an arity-1 bi-traversal [Bi] with
    floating left type. *)
module Chain_Traverse1_Bi1_left
    (LTrav : Traversable_types.S1)
    (Bi : Bi_traversable_types.Basic1_left) :
  Bi_traversable_types.S1_left
    with type 'l t = 'l LTrav.t Bi.t
     and type right = Bi.right

(** [Chain_Traverse1_Bi1_right (RTrav) (Bi)] composes an inner arity-1
    traversal [LTrav] on the right of an arity-1 bi-traversal [Bi] with
    floating right type. *)
module Chain_Traverse1_Bi1_right
    (RTrav : Traversable_types.S1)
    (Bi : Bi_traversable_types.Basic1_right) :
  Bi_traversable_types.S1_right
    with type 'r t = 'r RTrav.t Bi.t
     and type left = Bi.left
