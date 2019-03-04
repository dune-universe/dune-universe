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

open Base

(** Option extensions.

    This module contains various extensions for Core's [Option] module,
    including adding monadic traversal. *)

type 'a t = 'a option
(** [t] is just [option], re-exported to make this module satisfy
    various container interfaces. *)

include Traversable.S1_container with type 'a t := 'a t
(** Options are traversable containers. *)

include Filter_mappable.S1 with type 'a t := 'a t
(** Options are also filter-mappable; filter-mapping effectively
    behaves as monadic bind. *)

(** {2 Miscellaneous extension functions} *)

val first_some_of_thunks : (unit -> 'a t) list -> 'a t
(** [first_some_of_thunks thunks] evaluates each thunk in [thunks] until
    none remain (in which case, it returns [None], or one of the
    thunks returns [Some x] (in which case, it returns [Some x]. *)
