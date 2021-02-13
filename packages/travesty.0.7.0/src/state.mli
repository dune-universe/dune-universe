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

(** Haskell-style state monads.

    State monads are a way to thread a modifiable, readable state record
    through a computation without resorting to mutability. At any point in a
    state-monad computation, we can [peek] at the present value of the state,
    or [modify] it, or do both.

    We provide two signatures for state monads: one corresponding to the
    situation where the state type is fixed at the module level ({!S}), and
    one leaving the state type as part of the monad type ({!S2}). The former
    has a corresponding {{!Make} make functor}; the latter has a {{!M2}
    direct implementation}.

    We also provide a functor {!To_S} for fixing the state type in an arity-2
    monad after the fact. *)

(** {2 Relation to state transformers}

    Like in Haskell, state monads are a special case of {{!State_transform}
    state transformers}, corresponding to the application of those
    transformers to the identity monad.

    If you need to run stateful computations that can fail, or are partial,
    or non-deterministic, and so on, use state transformers instead. *)

(** {2 Signatures}

    For input and output module signatures for this module's functors, see
    {!State_types}. *)

(** {2 Manipulating state monads} *)

(** [To_S] flattens a {{!State_types.S2} S2} into an {{!State_types.S} S} by
    fixing the state type to [B.t]. *)
module To_S (M : State_types.S2) (B : Base.T) :
  State_types.S with type state = B.t and type 'a t = ('a, B.t) M.t

(** {2 Implementations and functors} *)

(** [Make] makes an {{!State_types.S} S} (state monad with fixed state type)
    from a single state type. *)
module Make (B : Base.T) : State_types.S with type state = B.t

(** [M2] is a basic implementation of {{!State_types.S2} S2} (state monad
    with variable state type). *)
module M2 : State_types.S2
