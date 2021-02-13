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

(** List extensions.

    This module contains various extensions for [Base]'s [List] module,
    including adding monadic traversal. *)

(** Defined to let this module be used directly in chaining operations etc. *)
type 'a t = 'a list

(** {2 Travesty instances} *)

(** Extended [With_errors], adding functions specific to working with lists
    in an error-prone context. *)
module With_errors : sig
  include
    Travesty.Traversable_types.S1_on_monad
      with type 'a t := 'a list
       and module M := Base.Or_error

  (** {3 Utility functions for modifying lists with error-prone functions} *)

  val replace_m :
    'a list -> int -> f:('a -> 'a option Or_error.t) -> 'a list Or_error.t
  (** [replace_m xs at ~f] tries to replace the value at index [at] in [xs]
      using the possibly-failing function [f]. [f] may return [Ok None], in
      which case the item is removed

      Examples:

      {[
        replace [1; 2; 3] 1
          ~f:(fun _ -> Ok None) (* Ok [1; 3] *)
          replace [1; 2; 3] 2
          ~f:(fun x -> Ok (Some (x + 1)))

        (* Ok [1; 2; 4] *)
      ]} *)
end

(** Lists are traversable containers, but have an extended [With_errors]
    submodule. *)
include
  Travesty.Traversable_types.S1
    with type 'a t := 'a t
     and module With_errors := With_errors

(** We can also filter-map over them. *)
include Travesty.Filter_mappable_types.S1 with type 'a t := 'a t

(** {2 Utility functions for modifying lists} *)

val replace : 'a list -> int -> f:('a -> 'a option) -> 'a list Or_error.t
(** [replace xs at ~f] tries to replace the value at index [at] in [xs] using
    the function [f]. [f] may return [None], in which case the item is
    removed.

    Examples:

    {[
      replace [1; 2; 3] 1
        ~f:(fun _ -> None) (* Ok [1; 3] *)
        replace [1; 2; 3] 2
        ~f:(fun x -> Some (x + 1))

      (* Ok [1; 2; 4] *)
    ]} *)

val insert : 'a list -> int -> 'a -> 'a list Or_error.t
(** [insert xs at value] tries to insert [value] at index [at] in [xs]. *)

(** {2 Miscellaneous extension functions} *)

val prefixes : 'a list -> 'a list list
(** [prefixes xs] returns all non-empty prefixes of [xs].

    Examples:

    {[
      prefixes [] (* [] *) prefixes [1; 2; 3]

      (* [ [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ] *)
    ]} *)
