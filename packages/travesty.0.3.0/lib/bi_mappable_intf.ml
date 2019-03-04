(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Signatures for (non-monadic) bi-mapping.

    The {{!sigs}main signatures} are {{!S2}S2}, {{!S1_left}S1_left},
    {{!S1_right}S1_right}, and {{!S0}S0}
    We also define various {{!exts}extension signatures}. *)

open Base

(** {2:generic The generic signature}

    As with {{!Traversable}Traversable}, we define the signature of
    bi-mappable structures in an arity-generic way, then specialise
    it for the various arities.
*)

(** [Generic] describes bi-mapping on any arity of type.

    - For arity-0 types, use {{!S0}S0}: [('l, 'r) t] becomes [t],
      ['l left] becomes [left], and ['r right] becomes [right].
    - For arity-1 types with a fixed right type, use
      {{!S1_left}S1_left}: [('l, 'r) t] becomes ['l t],
      ['l left] becomes ['l], and ['r right] becomes [right].
    - For arity-1 types with a fixed left type, use
      {{!S1_right}S1_right}: [('l, 'r) t] becomes ['l t],
      ['l left] becomes [left], and ['r right] becomes ['r].
    - For arity-2 types, use
      {{!S2}S2}: [('l, 'r) t] becomes [('l, 'r) t],
      ['l left] becomes ['l], and ['r right] becomes ['r].
*)
module type Generic = sig
  include Types_intf.Bi_generic

  val bi_map
    :  ('l1, 'r1) t
    -> left:('l1 left -> 'l2 left)
    -> right:('r1 right -> 'r2 right)
    -> ('l2, 'r2) t
  (** [bi_map c ~left ~right] maps [left] over every ['l1 left], and
      [right] over every ['r1 right], in [c]. *)
end

(** {2:sigs Basic signatures}

    The basic signatures are {{!S0}S0}, which defines mapping across an
    arity-0 type [t] (with a fixed, associated element type [elt]);
    {{!S1_left}S1_left} and {{!S1_right}S1_right}, which fix the
    right and left element type respectively (leaving the named type
    floating); and
    {{!S2}S2}, which defines mapping across an arity-2 type [('l, 'r) t]
    with left element type ['l] and right element type ['r]. *)

(** [S0] is the signature of an arity-0 bi-mappable type.

    Functions mapped over arity-0 types must preserve both element
   types. *)
module type S0 = sig
  include Types_intf.Bi0

  include Generic with type ('l, 'r) t := t
                   and type 'l left := left
                   and type 'r right := right
end

(** [S1_left] is the signature of an arity-1 bi-mappable type with a
   floating left type and fixed right type.

    Functions mapped over arity-1 types may change the left element
   type, but not the right. *)
module type S1_left = sig
  include Types_intf.Bi_left

  include Generic with type ('l, 'r) t := 'l t
                   and type 'l left := 'l
                   and type 'r right := right
end

(** [S1_right] is the signature of an arity-1 bi-mappable type with a
   floating right type and fixed left type.

    Functions mapped over arity-1 types may change the right element
   type, but not the left. *)
module type S1_right = sig
  include Types_intf.Bi_right

  include Generic with type ('l, 'r) t := 'r t
                   and type 'l left := left
                   and type 'r right := 'r
end

(** [S2] is the signature of an arity-2 bi-mappable type with
    floating left and right types. *)
module type S2 = sig
  include T2
  (** Type of containers. *)

  include Generic with type ('l, 'r) t := ('l, 'r) t
                   and type 'l left := 'l
                   and type 'r right := 'r
end

(** {2:exts Extensions}

    The signatures below describe various functions we can derive from
    bi-mappable types and mappable containers.  To apply them to existing
    types, use the functors in {{!Bi_mappable}Bi_mappable}. *)

(** [Generic_extensions] describes extensions that apply to all arities,
    in an arity-neutral manner. *)
module type Generic_extensions = sig
  include Types_intf.Bi_generic

  val map_left : ('l1, 'r) t -> f:('l1 left -> 'l2 left) -> ('l2, 'r) t
  (** [map_left c ~f] maps [f] over the left type of [c] only. *)

  val map_right : ('l, 'r1) t -> f:('r1 right -> 'r2 right) -> ('l, 'r2) t
  (** [map_right c ~f] maps [f] over the right type of [c] only. *)
end

(** Extensions for arity-0 bi-mappable containers. *)
module type Extensions0 = sig
  include Types_intf.Bi0

  include Generic_extensions
    with type ('l, 'r) t := t
     and type 'l left := left
     and type 'r right := right

  module Map_left : Mappable.S0 with type t := t
  (** Permits mapping over the left type. *)

  module Map_right : Mappable.S0 with type t := t
  (** Permits mapping over the right type. *)
end

(** Combines {{!S0}S0} and
   {{!Extensions0}Extensions0}. *)
module type S0_with_extensions = sig
  include S0

  include Extensions0
    with type t := t
     and type left := left
     and type right := right
end

(** Extensions for arity-1 bi-mappable containers with a floating left
   type. *)
module type Extensions1_left = sig
  include Types_intf.Bi_left

  include Generic_extensions
    with type ('l, 'r) t := 'l t
     and type 'l left := 'l
     and type 'r right := right

  module Fix_left (Left : T) : S0_with_extensions
    with type t := Left.t t
     and type left := Left.t
  (** Fixes the left type of this container to [Left.t]. *)

  include Mappable.S1 with type 'l t := 'l t
  (** Bi-mappable types with a fixed right type are mappable
      over their left. *)
end

(** Combines {{!S1_left}S1_left} and
   {{!Extensions1_left}Extensions1_left}. *)
module type S1_left_with_extensions = sig
  include S1_left
  include Extensions1_left
    with type 'l t := 'l t
     and type right := right
end

(** Extensions for arity-1 bi-mappable containers with a floating right
   type. *)
module type Extensions1_right = sig
  include Types_intf.Bi_right

  include Generic_extensions
    with type ('l, 'r) t := 'r t
     and type 'l left := left
     and type 'r right := 'r

  module Fix_right (Right : T) : S0_with_extensions
    with type t := Right.t t
     and type right := Right.t
  (** Fixes the right type of this container to [Right.t]. *)

  include Mappable.S1 with type 'r t := 'r t
  (** Bi-mappable types with a fixed left type are mappable
      over their right. *)
end

(** Combines {{!S1_right}S1_right} and
   {{!Extensions1_right}Extensions1_right}. *)
module type S1_right_with_extensions = sig
  include S1_right
  include Extensions1_right
    with type 'r t := 'r t
     and type left := left
end

(** [Extensions2] describes various extensions of arity-1 mappable
   containers. *)
module type Extensions2 = sig
  include T2

  include Generic_extensions with type ('l, 'r) t := ('l, 'r) t
                              and type 'l left := 'l
                              and type 'r right := 'r

  (** To fix both types, use [T.Fix_left(Left).Fix_right(right)]. *)

  module Fix_left (Left : T) : S1_right_with_extensions
    with type 'r t := (Left.t, 'r) t
     and type left := Left.t
  (** Fixes the left type of this container to [Left.t]. *)

  module Fix_right (Right : T) : S1_left_with_extensions
    with type 'l t := ('l, Right.t) t
     and type right := Right.t
  (** Fixes the right type of this container to [Right.t]. *)
end

(** Combines {{!S2}S2} and {{!Extensions2}Extensions2}. *)
module type S2_with_extensions = sig
  include S2
  include Extensions2 with type ('l, 'r) t := ('l, 'r) t
end
