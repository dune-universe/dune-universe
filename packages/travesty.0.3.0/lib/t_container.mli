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

include module type of T_container_intf
(** As usual, we store the extension signatures in a separate
    {{!T_container_intf}intf module}. *)

(** {2:functors Extension functors}

    These functors extend Core containers with the extensions
    described in {{!T_container_intf.Extensions0}Extensions0} and
    {{!T_container_intf.Extensions1}Extensions1}.
*)

module Extend0 (C : Container.S0) : Extensions0 with type t := C.t
                                                 and type elt := C.elt
(** [Extend0] creates {{!T_container_intf.Extensions0}extensions} for a
    [Container.S0]. *)

module Extend0_predicate
    (P : T) (C : Container.S0 with type elt = (P.t -> bool))
  : Extensions0_predicate with type t := C.t and type item := P.t
  (** [Extend0_predicate] creates
     {{!T_container_intf.Extensions0_predicate}extensions} for a
     [Container.S0] over predicates. *)

module Extend1 (C : Container.S1) : Extensions1 with type 'a t := 'a C.t
(** [Extend1] creates {{!T_container_intf.Extensions1}extensions} for a
    [Container.S1]. *)
