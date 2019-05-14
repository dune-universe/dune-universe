(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

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

(** Extensions for containers.

    We keep these in the main Travesty library because it's useful to pull
    them in for {{!Mappable} mappable} and {{!Traversable} traversable}
    containers. *)

open Base

(** As usual, we store the extension signatures in a separate
    {{!Container_exts_intf} intf module}. *)
include module type of Container_exts_intf

(** {2:functors Extension functors}

    These functors extend Core containers with the extensions described in
    {{!Container_exts_intf.S0} S0} and {{!Container_exts_intf.S1} S1}. *)

(** [Extend0] creates {{!Container_exts_intf.S0} extensions} for a
    [Container.S0]. *)
module Extend0 (C : Container.S0) :
  S0 with type t := C.t and type elt := C.elt

(** [Extend0_predicate] creates
    {{!Container_exts_intf.S0_predicate} extensions} for a [Container.S0]
    over predicates. *)
module Extend0_predicate
    (P : T)
    (C : Container.S0 with type elt = P.t -> bool) :
  S0_predicate with type t := C.t and type item := P.t

(** [Extend1] creates {{!Container_exts_intf.S1} extensions} for a
    [Container.S1]. *)
module Extend1 (C : Container.S1) : S1 with type 'a t := 'a C.t
