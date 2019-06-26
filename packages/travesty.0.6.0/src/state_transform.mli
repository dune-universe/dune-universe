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

(** Haskell-style state transformers.

    State transformers generalise state monads. They attach onto an existing
    monad, and allow the stateful computations to return and handle values
    inside that monad. For example, transforming [On_error] provides
    stateful, potentially-failing computations.

    We provide two signatures for state transformers: one corresponding to
    the situation where the state type is fixed at the module level
    ({{!S} S}), and one leaving the state type as part of the monad type
    ({{!S2} S2}). Both have corresponding {{!make} make functors}.

    We also provide a functor {{!To_S} To_S} for fixing the state type in an
    arity-2 monad after the fact. *)

open Base

(** {2 Signatures}

    For input and output signatures for this module's functors, see
    {{!State_transform_types} State_transform_types}. *)

(** {2 Manipulating state transformers} *)

(** [To_S] flattens a {{!State_transform_types.S2} S2} into an
    {{!State_transform_types.S} S} by fixing the state type to [B.t]. *)
module To_S (M : State_transform_types.S2) (B : Base.T) :
  State_transform_types.S
  with type state = B.t
   and type 'a t = ('a, B.t) M.t
   and module Inner = M.Inner

(** {2:make Functors for making state transformers} *)

(** [Make] makes an {{!State_transform_types.S} S} (state transformer with
    fixed state type) from a {{!State_transform_types.Basic} Basic}. *)
module Make (B : State_transform_types.Basic) :
  State_transform_types.S with type state = B.t and module Inner = B.Inner

(** [Make2] makes an {{!State_transform_types.S2} S2} (state transformer
    with variable state type) from a [Monad.S]. *)
module Make2 (M : Monad.S) : State_transform_types.S2 with module Inner = M
