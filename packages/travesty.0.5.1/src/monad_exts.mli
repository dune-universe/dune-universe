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

(** Generic monad extensions.

    [Monad_exts] contains a signature and functor for adding various
    {{!exts} extensions} to [Base]-style monads. *)

open Base

(** {2:exts Extensions} *)

(** As is often the case, we define the extension signatures in a separate
    file. *)
include module type of Monad_exts_intf

(** [Extend] creates {{!S} extensions} for a [Monad.S]. *)
module Extend (M : Monad.S) : S with type 'a t := 'a M.t

(** {2:misc Miscellaneous} *)

(** Demotes an arity-2 monad [M] to an arity-1 one, fixing its second type
    to be [B.t]. *)
module S2_to_S (M : Monad.S2) (B : T) :
  Monad.S with type 'a t := ('a, B.t) M.t

(** Converts a monad to a mappable over [M.map].

    At time of writing, [M] satisfies
    {{!Mappable.S1} the mappable interface} in its own right; this functor
    mainly exists as insurance in case the two interfaces ever diverge. *)
module To_mappable (M : Monad.S) : Mappable.S1 with type 'a t := 'a M.t
