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

open Core_kernel

include State_intf

(* We implement all of the various monads and functors in terms of
   [State_transform] and the identity monad, for simplicity.  This
   reflects the situation in Haskell, except that Haskell has curried
   type constructors and we have separate arity modules. *)

module M2 : S2 = State_transform.Make2 (Monad.Ident)

module To_S (M : S2) (B : Base.T)
  : S with type state = B.t
       and type 'a t = ('a, B.t) M.t = struct
  type state = B.t

  module M1 = struct
    type 'a t = ('a, state) M.t
    include T_monad.S2_to_S (M) (B)
  end
  include M1
  include T_monad.Extend (M1)

  include (M : Generic with type ('a, 's) t := 'a t
                        and type 'a final := 'a
                        and type 's state := state)
end

module Make (B : T) : S with type state = B.t =
  State_transform.Make (struct
    type t = B.t
    module Inner = Monad.Ident
  end)
;;
