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

(** Generic type signatures.

    [Types_intf] contains several signatures that contain types for
    container-like structures.  These are intended for inclusion in
    other signatures. *)

(** {2 Mappable and traversable} *)

(** [Generic] defines the types used in arity-generic container-like
    signatures. *)
module type Generic = sig
  type 'a t
  (** Placeholder for the container type. *)

  type 'a elt
  (** Placeholder for the type of elements inside the container. *)
end

(** [S0] defines the types used in arity-0 container-like signatures. *)
module type S0 = sig
  type t
  (** The container type. *)

  type elt
  (** The element type. *)
end

(** There is no [S1], as Core's [T1] has the same effect. *)

(** {2 Bi-mappable and bi-traversable} *)

(** Types used in generic signatures over bi-operations. *)
module type Bi_generic = sig
  type ('l, 'r) t
  (** Generic container type. *)

  type 'l left
  (** Generic left type. *)

  type 'r right
  (** Generic right type. *)
end

(** Types used in leftwards arity-1 bi-operation signatures. *)
module type Bi_left = sig
  type 'l t
  (** Partially fixed type of containers. *)

  type right
  (** Fixed type of right elements. *)
end

(** Types used in rightwards arity-1 bi-operation signatures. *)
module type Bi_right = sig
  type 'r t
  (** Partially fixed type of containers. *)

  type left
  (** Fixed type of left elements. *)
end

(** Types used in arity-0 bi-operation signatures. *)
module type Bi0 = sig
  type t
  (** Fixed type of containers. *)

  type left
  (** Fixed type of left elements. *)

  type right
  (** Fixed type of right elements. *)
end

(** There is no [Bi2], as Core's [T2] has the same effect. *)
