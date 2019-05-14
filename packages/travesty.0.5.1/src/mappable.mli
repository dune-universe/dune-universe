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

(** Non-monadic mapping.

    [Mappable] contains signatures and extensions for types that can be
    (non-monadically) mapped over. It resembles the Haskell (but not the
    OCaml!) notion of a functor, though we call the mapping function [map]. *)

(** {2 Signatures} *)

(** {{!Mappable_intf} Mappable_intf} contains the signatures for [Mappable]. *)
include module type of Mappable_intf

(** {2 Extending mappable containers}

    We define several derived functions for mappable containers in
    {{!Mappable_intf} Mappable_intf}---here, we define functors to generate
    them. *)

(** [Extend1] implements [Extensions1] for an arity-1 mappable container. *)
module Extend1 (S : S1_container) : Extensions1 with type 'a t := 'a S.t
