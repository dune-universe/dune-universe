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

open Base

module type Extensions = sig
  type 'a t

  val when_m   : bool -> f:(unit -> unit t) -> unit t
  val unless_m : bool -> f:(unit -> unit t) -> unit t
end

module Extend (M : Monad.S) : Extensions with type 'a t := 'a M.t = struct
  let when_m predicate ~f = if predicate then f () else M.return ()
  let unless_m predicate ~f = if predicate then M.return () else f ()
end

module S2_to_S (M : Monad.S2) (B : T)
  : Monad.S with type 'a t := ('a, B.t) M.t =
  Monad.Make (struct
    type 'a t = ('a, B.t) M.t
    let map = `Custom M.map
    let return = M.return
    let bind = M.bind
  end)
;;
