(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

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

(** Generic type signatures.

    This module contains several signatures that contain types for
    container-like structures. These are intended for inclusion in other
    signatures. *)

(** {2 Mappable and traversable} *)

(** [Generic] defines the types used in arity-generic container-like
    signatures. *)
module type Generic = sig
  (** Placeholder for the container type. *)
  type 'a t

  (** Placeholder for the type of elements inside the container. *)
  type 'a elt
end

(** [S0] defines the types used in arity-0 container-like signatures. *)
module type S0 = sig
  (** The container type. *)
  type t

  (** The element type. *)
  type elt
end

(** There is no [S1], as Core's [T1] has the same effect. *)

(** {2 Bi-mappable and bi-traversable} *)

(** Types used in generic signatures over bi-operations. *)
module type Bi_generic = sig
  (** Generic container type. *)
  type ('l, 'r) t

  (** Generic left type. *)
  type 'l left

  (** Generic right type. *)
  type 'r right
end

(** Types used in leftwards arity-1 bi-operation signatures. *)
module type Bi_left = sig
  (** Partially fixed type of containers. *)
  type 'l t

  (** Fixed type of right elements. *)
  type right
end

(** Types used in rightwards arity-1 bi-operation signatures. *)
module type Bi_right = sig
  (** Partially fixed type of containers. *)
  type 'r t

  (** Fixed type of left elements. *)
  type left
end

(** Types used in arity-0 bi-operation signatures. *)
module type Bi0 = sig
  (** Fixed type of containers. *)
  type t

  (** Fixed type of left elements. *)
  type left

  (** Fixed type of right elements. *)
  type right
end

(** There is no [Bi2], as Core's [T2] has the same effect. *)
