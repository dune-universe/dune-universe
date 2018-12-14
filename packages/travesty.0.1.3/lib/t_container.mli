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

(** Extensions for containers. *)

open Core_kernel

(** {2:a1 Arity-1 container extensions}

    These extensions target arity-1 containers (implementations of
    [Container.S1]).
*)

(** [Extensions1] contains extensions for a [Container.S1]. *)
module type Extensions1 = sig
  type 'a t
  (** The type of the container to extend. *)

  val max_measure : measure:('a -> int) -> ?default:int -> 'a t -> int
  (** [max_measure ~measure ~default xs] measures each item in [xs]
      according to [measure], and returns the highest measure reported.
      If [xs] is empty, return [default] if given, and [0]
      otherwise. *)

  val any : predicates:('a -> bool) t -> 'a -> bool
  (** [any ~predicates x] tests [x] against [predicates] until one
      returns [true], or all return [false]. *)
end

module Extend1 (C : Container.S1) : Extensions1 with type 'a t := 'a C.t
(** [Extend1] creates {{!Extensions}Extensions} for a [Container.S1]. *)
