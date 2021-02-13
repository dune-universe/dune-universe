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

    - {{!basic} Basic{i n}}: minimal definition of bi-mappable modules;
    - {{!s} S{i n}}: full bi-mappable containers, produced by applying
      functors to the above.

    We also define other signatures, mostly for internal book-keeping. They
    may be useful elsewhere, however. *)

open Base

(** {3:basic Basic signatures} *)

(** {4:generic The generic signature}

    As with {!Traversable}, we define the basic signature of bi-mappable
    structures in an arity-generic way, then specialise it for the various
    arities. *)

(** [Basic_generic] describes bi-mapping on any arity of type.

    - For arity-0 types, use {!S0}: [('l, 'r) t] becomes [t], ['l left]
      becomes [left], and ['r right] becomes [right].
    - For arity-1 types with a fixed right type, use {!S1_left}: [('l, 'r) t]
      becomes ['l t], ['l left] becomes ['l], and ['r right] becomes [right].
    - For arity-1 types with a fixed left type, use {!S1_right}: [('l, 'r) t]
      becomes ['l t], ['l left] becomes [left], and ['r right] becomes ['r].
    - For arity-2 types, use {!S2}: [('l, 'r) t] becomes [('l, 'r) t],
      ['l left] becomes ['l], and ['r right] becomes ['r]. *)
module type Basic_generic = sig
  include Generic_types.Bi_generic

  val bi_map :
       ('l1, 'r1) t
    -> left:('l1 left -> 'l2 left)
    -> right:('r1 right -> 'r2 right)
    -> ('l2, 'r2) t
  (** [bi_map c ~left ~right] maps [left] over every ['l1 left], and [right]
      over every ['r1 right], in [c]. *)
end

(** {4:sigs Arity-specific basic signatures}

    The basic signatures are {!Basic0}, which defines mapping across an
    arity-0 type [t] (with a fixed, associated element type [elt]);
    {!Basic1_left} and {!Basic1_right}, which fix the right and left element
    type respectively (leaving the named type floating); and {!Basic2}, which
    defines mapping across an arity-2 type [('l, 'r) t] with left element
    type ['l] and right element type ['r]. *)

(** [Basic0] is the basic signature of an arity-0 bi-mappable type.

    Functions mapped over arity-0 types must preserve both element types. *)
module type Basic0 = sig
  include Generic_types.Bi0

  include
    Basic_generic
      with type ('l, 'r) t := t
       and type 'l left := left
       and type 'r right := right
end

(** [Basic1_left] is the basic signature of an arity-1 bi-mappable type with
    a floating left type and fixed right type.

    Functions mapped over arity-1 types may change the left element type, but
    not the right. *)
module type Basic1_left = sig
  include Generic_types.Bi_left

  include
    Basic_generic
      with type ('l, 'r) t := 'l t
       and type 'l left := 'l
       and type 'r right := right
end

(** [Basic1_right] is the signature of an arity-1 bi-mappable type with a
    floating right type and fixed left type.

    Functions mapped over arity-1 types may change the right element type,
    but not the left. *)
module type Basic1_right = sig
  include Generic_types.Bi_right

  include
    Basic_generic
      with type ('l, 'r) t := 'r t
       and type 'l left := left
       and type 'r right := 'r
end

(** [Basic2] is the signature of an arity-2 bi-mappable type with floating
    left and right types. *)
module type Basic2 = sig
  (** Type of containers. *)
  include T2

  include
    Basic_generic
      with type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l
       and type 'r right := 'r
end

(** {3:s Signatures for bi-mappable types}

    The signatures below include various functions we can derive from
    bi-mappable types. *)

(** [Generic] is a generic interface for bi-mappable types, used to build
    [S0] (arity-0) and [S1] (arity-1). *)
module type Generic = sig
  include Basic_generic

  val map_left : ('l1, 'r) t -> f:('l1 left -> 'l2 left) -> ('l2, 'r) t
  (** [map_left c ~f] maps [f] over the left type of [c] only. *)

  val map_right : ('l, 'r1) t -> f:('r1 right -> 'r2 right) -> ('l, 'r2) t
  (** [map_right c ~f] maps [f] over the right type of [c] only. *)
end

(** [S0] is the full signature of an arity-0 bi-mappable type.

    Functions mapped over arity-0 types must preserve both element types. *)
module type S0 = sig
  include Generic_types.Bi0

  include
    Generic
      with type ('l, 'r) t := t
       and type 'l left := left
       and type 'r right := right
end

(** [S1_left] is the full signature of an arity-1 bi-mappable type with a
    floating left type and fixed right type.

    Functions mapped over arity-1 types may change the left element type, but
    not the right. *)
module type S1_left = sig
  include Generic_types.Bi_left

  include
    Generic
      with type ('l, 'r) t := 'l t
       and type 'l left := 'l
       and type 'r right := right
end

(** [S1_right] is the full signature of an arity-1 bi-mappable type with a
    floating right type and fixed left type.

    Functions mapped over arity-1 types may change the right element type,
    but not the left. *)
module type S1_right = sig
  include Generic_types.Bi_right

  include
    Generic
      with type ('l, 'r) t := 'r t
       and type 'l left := left
       and type 'r right := 'r
end

(** [S2] is the full signature of an arity-2 bi-mappable type with floating
    left and right types. *)
module type S2 = sig
  (** Type of containers. *)
  include T2

  include
    Generic
      with type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l
       and type 'r right := 'r
end
