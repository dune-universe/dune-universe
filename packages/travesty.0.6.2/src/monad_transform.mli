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

(** Monad transformer signatures.

    These signatures capture the monad transformer pattern in a generic way. *)

open Base

(** [S_fixed] is the signature of monad transformers fixed to a particular
    inner monad. *)
module type S_fixed = sig
  type 'a t
  (** The type of the outer transformer. *)

  module Inner : Monad.S
  (** [Inner] is the inner monad. *)

  val lift : 'a Inner.t -> 'a Inner.t t
  (** [lift x] lifts [x] from the inner monad to the outer one. *)
end

(** [S] is the signature of monad transformers. *)
module type S = sig
  type 'a t
  (** The type of the outer transformer. *)

  (** [On_monad] is a functor that transforms its argument monad. *)
  module On_monad (M : Monad.S) :
    S_fixed with type 'a t := 'a t and module Inner := M
end
