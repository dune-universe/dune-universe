(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Option extensions.

    This module contains various extensions for [Base]'s [Option] module,
    including adding monadic traversal. *)

(** Defined to let this module be used directly in chaining operations etc. *)
type 'a t = 'a option

(** {2 Travesty instances} *)

(** Options are traversable containers. *)
include Travesty.Traversable_types.S1 with type 'a t := 'a t

(** Options are also filter-mappable; filter-mapping effectively behaves as
    monadic bind. *)
include Travesty.Filter_mappable_types.S1 with type 'a t := 'a t

(** Finally, options are a monad, and take the various monad extensions. *)
include Travesty.Monad_exts_types.S with type 'a t := 'a t

(** {3 Applying defaults non-eagerly} *)

val value_f : 'a option -> default_f:(unit -> 'a) -> 'a
(** [value_f opt ~default_f] behaves like
    [value opt ~default:(default_f ())], but only evaluates the thunk
    [default_f] if [value] is None. *)

val value_l : 'a option -> default_l:'a Lazy.t -> 'a
(** [value_f opt ~default_l] behaves like
    [value opt ~default:(Lazy.force default_l)], but only forces [default_l]
    if [value] is None. *)

(** {2 Miscellaneous extension functions} *)

val first_some_of_thunks : (unit -> 'a option) list -> 'a option
(** [first_some_of_thunks thunks] evaluates each thunk in [thunks] until
    none remain (in which case, it returns [None], or one of the thunks
    returns [Some x] (in which case, it returns [Some x]. *)
