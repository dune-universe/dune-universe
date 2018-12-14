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

(** List extensions.

    This module contains various extensions for Core's [List] module, including
    adding monadic traversal. *)

open Base

type 'a t = 'a list
(** [t] is just [list], re-exported to make this module satisfy
    various container interfaces. *)

include Traversable.S1_container with type 'a t := 'a t
(** Lists are traversable containers. *)

(** {2 Miscellaneous extension functions} *)

val exclude : f:('a -> bool) -> 'a t -> 'a t
(** [exclude ~f xs] is the inverse of [filter ~f xs]. *)

val prefixes : 'a t -> 'a t t
(** [prefixes xs] returns all non-empty prefixes of [xs]. *)

val one : 'a list -> 'a Or_error.t
(** [one xs] returns [Ok x] if [xs] is a list containing only [x],
    and an error otherwise. *)

val two : 'a list -> ('a * 'a) Or_error.t
(** [two xs] returns [Ok (x, y)] if [xs] is a list containing only [x]
    and [y] in that order, and an error otherwise. *)
