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

(** Signatures for (non-monadic) mapping.

    The {{!sigs}main signatures} are {{!S0}S0} and {{!S1}S1}.
    We also define {{!containers}container} forms of the above, which include
    the Core container interfaces, and {{!Extensions1}Extensions1}, a
    signature of extensions to arity-1 mappable containers. *)

open Base

(** {2:generic The generic signature}

    As with {{!Traversable}Traversable}, we define the signature of
    mappable structures in an arity-generic way, then specialise
    it for arity-0 and arity-1 types.
*)

(** [Generic] describes mapping on either an arity-0 or
   arity-1 type.

    - For arity-0 types, use {{!S0}S0}: ['a t] becomes [t], and
      ['a elt] becomes [elt];
    - For arity-1 types, use {{!S1}S1}: ['a t] becomes ['a t],
      and ['a elt] becomes ['a].
*)
module type Generic = sig
  include Types_intf.Generic
  (** [Generic] refers to the container type as ['a t], and the element type
      as ['a elt]; substitute [t]/[elt] (arity-0) or ['a t]/['a] (arity-1)
      accordingly below. *)

  val map : 'a t -> f:('a elt -> 'b elt) -> 'b t
  (** [map c ~f] maps [f] over every [t] in [c]. *)
end

(** {2:sigs Basic signatures}

    The basic signatures are {{!S0}S0}, which defines mapping across an
    arity-0 type [t] (with a fixed, associated element type [elt]), and
    {{!S1}S1}, which defines mapping across an arity-1 type ['a t] (with
    element type ['a]). *)

(** [S0] is the signature of an arity-0 mappable type.

    Functions mapped over arity-0 types must preserve the element
   type.  *)
module type S0 = sig
  include Types_intf.S0
  include Generic with type 'a t := t and type 'a elt := elt
end

(** [S1] is the signature of an arity-1 mappable type.

    Functions mapped over arity-1 types may change the element type. *)
module type S1 = sig
  (** [t] is the type of the container to map over. *)
  type 'a t

  include Generic with type 'a t := 'a t and type 'a elt := 'a
end

(** {2:containers Mappable container signatures}

    Unlike with {{!Traversable}Traversable}'s [map_m], we can't actually
    implement the Core container signatures over [map] alone.  We still
    define versions of the [Mappable] interfaces that include their respective
    container signatures, both for symmetry and to allow for extensions.  *)

(** [S0_container] is the signature of an arity-0 mappable container. *)
module type S0_container = sig
  include S0
  include Container.S0 with type t := t and type elt := elt
end

(** [S1_container] is the signature of an arity-1 mappable container. *)
module type S1_container = sig
  include S1
  include Container.S1 with type 'a t := 'a t
end

(** {2 Extensions}

    The signatures below describe various functions we can derive from
    mappable types and mappable containers.  To apply them to existing
    types, use the functors in {{!Mappable}Mappable}. *)

(** [Extensions1] describes various extensions of arity-1 mappable
   containers. *)
module type Extensions1 = sig
  type 'a t
  (** [t] is the type of the container to map over. *)

  include T_container.Extensions1 with type 'a t := 'a t
    (** [Extensions1] includes the container extensions from
        {{!T_container}T_container}, as they work with any arity-1
        container. *)

  val right_pad : padding:'a -> 'a list t -> 'a list t
  (** [right_pad ~padding xs] pads every list in xs with [padding],
      ensuring all lists have equal length. *)
end
