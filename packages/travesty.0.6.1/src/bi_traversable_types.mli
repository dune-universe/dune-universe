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

(** The main groups of signatures provided by this module are:

    - {{!basic} Basic{i n}}: minimal definition of bi-traversable modules;
    - {{!s} S{i n}}: full bi-traversable containers, produced by applying
      functors to the above.

    We also define other signatures, mostly for internal book-keeping. They
    may be useful elsewhere, however. *)

open Base

(** {3 Inner-traversal signatures}

    These signatures form the inner body of the [On_monad] functor in the
    main signatures. They all have names ending with [_on_monad], and assume
    the existence of a monad [M].

    While they aren't interesting on their own, they do contain (in slightly
    abstract form) the specific functions needed to build, and provided on
    building, bi-traversable containers. *)

(** {4:omgeneric The generic signatures}

    Here, wee define some signatures for bi-traversable structures in an
    arity-generic way. We then specialise them for arity-0 and arity-1 types. *)

(** [Basic_generic_on_monad] describes monadic bi-traversal on types of any
    arity.

    - For arity-0 types, [('l, 'r) t] becomes [t], ['l left] becomes [left],
      and ['r right] becomes [right].
    - For arity-1 types with a fixed right type, [('l, 'r) t] becomes ['l t],
      ['l left] becomes ['l], and ['r right] becomes [right].
    - For arity-1 types with a fixed left type, [('l, 'r) t] becomes ['l t],
      ['l left] becomes [left], and ['r right] becomes ['r].
    - For arity-2 types, [('l, 'r) t] becomes [('l, 'r) t], ['l left] becomes
      ['l], and ['r right] becomes ['r]. *)
module type Basic_generic_on_monad = sig
  include Generic_types.Bi_generic

  module M : Monad.S
  (** [M] is the monad over which we're bi-traversing. *)

  val bi_map_m :
       ('l1, 'r1) t
    -> left:('l1 left -> 'l2 left M.t)
    -> right:('r1 right -> 'r2 right M.t)
    -> ('l2, 'r2) t M.t
  (** [bi_map_m c ~left ~right] monadically traverses with [left] over every
      ['l1 left], and [right] over every ['r1 right], in [c]. *)
end

(** [Generic_on_monad] extends [Generic] to contain various derived
    operators; we use it to derive the signatures of the various [On_monad]
    modules.

    - For arity-0 types, [('l, 'r) t] becomes [t], ['l left] becomes [left],
      and ['r right] becomes [right].
    - For arity-1 types with a fixed right type, [('l, 'r) t] becomes ['l t],
      ['l left] becomes ['l], and ['r right] becomes [right].
    - For arity-1 types with a fixed left type, [('l, 'r) t] becomes ['l t],
      ['l left] becomes [left], and ['r right] becomes ['r].
    - For arity-2 types, [('l, 'r) t] becomes [('l, 'r) t], ['l left] becomes
      ['l], and ['r right] becomes ['r]. *)
module type Generic_on_monad = sig
  include Basic_generic_on_monad

  val map_left_m :
    ('l1, 'r) t -> f:('l1 left -> 'l2 left M.t) -> ('l2, 'r) t M.t
  (** [map_left_m c ~f] monadically traverses over the left type of [c] only,
      using [f]. *)

  val map_right_m :
    ('l, 'r1) t -> f:('r1 right -> 'r2 right M.t) -> ('l, 'r2) t M.t
  (** [map_right_m c ~f] monadically traverses [f] over the right type of [c]
      only, using [f]. *)
end

(** {4:ombasic Basic signatures} *)

(** [Basic0_on_monad] is the inner signature of a monadic bi-traversal over
    arity-0 types. *)
module type Basic0_on_monad = sig
  include Generic_types.Bi0

  include
    Basic_generic_on_monad
      with type ('l, 'r) t := t
       and type 'l left := left
       and type 'r right := right
end

(** [Basic1_left_on_monad] is the inner signature of a monadic bi-traversal
    over arity-1 types with a floating left type and fixed right type. *)
module type Basic1_left_on_monad = sig
  include Generic_types.Bi_left

  include
    Basic_generic_on_monad
      with type ('l, 'r) t := 'l t
       and type 'l left := 'l
       and type 'r right := right
end

(** [Basic1_right_on_monad] is the inner signature of a monadic bi-traversal
    over arity-1 types with a floating right type and fixed left type. *)
module type Basic1_right_on_monad = sig
  include Generic_types.Bi_right

  include
    Basic_generic_on_monad
      with type ('l, 'r) t := 'r t
       and type 'l left := left
       and type 'r right := 'r
end

(** [Basic2_on_monad] is the inner signature of a monadic bi-traversal over
    arity-2 types with a floating right type and fixed left type. *)
module type Basic2_on_monad = sig
  include T2

  include
    Basic_generic_on_monad
      with type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l
       and type 'r right := 'r
end

(** {3:basic Basic signatures}

    We now define basic signatures that generalise the above signatures over
    all monads.

    These signatures form the input to functors that provide derived
    operations, chaining, type-fixing, and conversion to bi-mappable and
    regular traversable containers.

    The basic signatures are {{!Basic0} Basic0}, which defines traversal
    across an arity-0 type [t] (with a fixed, associated element type [elt]);
    {{!Basic1_left} Basic1_left} and {{!Basic1_right} Basic1_right}, which
    fix the right and left element type respectively (leaving the named type
    floating); and {{!Basic2} Basic2}, which defines traversal across an
    arity-2 type [('l, 'r) t] with left element type ['l] and right element
    type ['r]. *)

(** [Basic0] is the basic signature of an arity-0 bi-traversable type.

    Functions traversing over arity-0 types must preserve both element types. *)
module type Basic0 = sig
  include Generic_types.Bi0

  (** [On_monad] implements monadic bi-traversal for a given monad. *)
  module On_monad (M : Monad.S) :
    Basic0_on_monad
      with type t := t
       and type left := left
       and type right := right
       and module M := M
end

(** [Basic1_left] is the basic signature of an arity-1 bi-traversable type
    with a floating left type and fixed right type.

    Functions traversing over arity-1 types may change the left element type,
    but not the right. *)
module type Basic1_left = sig
  include Generic_types.Bi_left

  (** [On_monad] implements monadic bi-traversal for a given monad. *)
  module On_monad (M : Monad.S) :
    Basic1_left_on_monad
      with type 'r t := 'r t
       and type right := right
       and module M := M
end

(** [Basic1_right] is the basic signature of an arity-1 bi-traversable type
    with a floating right type and fixed left type.

    Functions traversing over arity-1 types may change the right element
    type, but not the left. *)
module type Basic1_right = sig
  include Generic_types.Bi_right

  (** [On_monad] implements monadic bi-traversal for a given monad. *)
  module On_monad (M : Monad.S) :
    Basic1_right_on_monad
      with type 'l t := 'l t
       and type left := left
       and module M := M
end

(** [Basic2] is the signature of an arity-2 bi-traversable type with floating
    left and right types. *)
module type Basic2 = sig
  include T2

  (** [On_monad] implements monadic bi-traversal for a given monad. *)
  module On_monad (M : Monad.S) :
    Basic2_on_monad with type ('l, 'r) t := ('l, 'r) t and module M := M
end

(** {3:s Signatures for bi-traversable types}

    The signatures below include various functions we can derive from
    bi-mappable types. *)

(** [Generic] is a generic interface for bi-mappable types, used to build
    [S0] (arity-0) and [S1] (arity-1). *)
module type Generic = sig
  include Generic_types.Bi_generic

  (** We can do non-monadic bi-mapping operations. *)
  include
    Bi_mappable_types.Generic
      with type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l left
       and type 'r right := 'r right

  (** [On_monad] implements monadic bi-traversal operators for a given monad
      [M]. *)
  module On_monad (M : Monad.S) :
    Generic_on_monad
      with type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l left
       and type 'r right := 'r right
       and module M := M

  (** [With_errors] specialises [On_monad] to the error_monad. *)
  module With_errors :
    Generic_on_monad
      with type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l left
       and type 'r right := 'r right
       and module M := Or_error
end

(** [S0] is the full signature of an arity-0 bi-traversable type.

    Functions traversing over arity-0 types must preserve both element types. *)
module type S0 = sig
  include Generic_types.Bi0

  include
    Generic
      with type ('l, 'r) t := t
       and type 'l left := left
       and type 'r right := right
end

(** [S1_left] is the full signature of an arity-1 bi-traversable type with a
    floating left type and fixed right type.

    Functions traversing over arity-1 types may change the left element type,
    but not the right. *)
module type S1_left = sig
  include Generic_types.Bi_left

  include
    Generic
      with type ('l, 'r) t := 'l t
       and type 'l left := 'l
       and type 'r right := right
end

(** [S1_right] is the full signature of an arity-1 bi-traversable type with a
    floating right type and fixed left type.

    Functions traversing over arity-1 types may change the right element
    type, but not the left. *)
module type S1_right = sig
  include Generic_types.Bi_right

  include
    Generic
      with type ('l, 'r) t := 'r t
       and type 'l left := left
       and type 'r right := 'r
end

(** [S2] is the full signature of an arity-2 bi-traversable type with
    floating left and right types. *)
module type S2 = sig
  include T2

  include
    Generic
      with type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l
       and type 'r right := 'r
end
