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

open Base

(** [Generic] contains the signature bits common to all state monad
    signatures. *)
module type Generic = sig
  (** State monads share the signatures of their builder functions with state
      transformers... *)
  include State_transform_types.Generic_builders

  (** ...as well as their runner functions... *)
  include
    State_transform_types.Generic_runners
      with type ('a, 's) t := ('a, 's) t
       and type 'a final := 'a final
       and type 's state := 's state

  (** ...and fixed-point combinators. *)
  include State_transform_types.Fix with type ('a, 's) t := ('a, 's) t
end

(** [S] is the signature of state monads parametrised over their value, but
    with a fixed state type. *)
module type S = sig
  (** The fixed state type. *)
  type state

  include Monad.S

  include Monad_exts_types.S with type 'a t := 'a t

  include
    Generic
      with type ('a, 's) t := 'a t
       and type 's state := state
       and type 'a final := 'a
end

(** [S2] is the signature of state monads parametrised over both value and
    state types. *)
module type S2 = sig
  include Monad.S2

  include
    Generic
      with type ('a, 's) t := ('a, 's) t
       and type 's state := 's
       and type 'a final := 'a
end
