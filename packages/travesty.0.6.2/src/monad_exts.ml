(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

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
open Monad_exts_types

module Extend (M : Monad.S) : S with type 'a t := 'a M.t = struct
  let then_m (x : _ M.t) (y : 'a M.t) : 'a M.t = M.(x >>= fun _ -> y)

  let compose_m (f : 'a -> 'b M.t) (g : 'b -> 'c M.t) (x : 'a) : 'c M.t =
    M.(x |> f >>= g)

  let ( >> ) = then_m

  let ( >=> ) = compose_m

  let map_when_m ?(otherwise : 'a -> 'a M.t = M.return) (condition : bool)
      (a : 'a) ~(f : 'a -> 'a M.t) : 'a M.t =
    (if condition then f else otherwise) a

  let when_m ?(otherwise : (unit -> unit M.t) option) (condition : bool)
      ~(f : unit -> unit M.t) : unit M.t =
    map_when_m ?otherwise condition ~f ()

  let map_unless_m ?(otherwise : ('a -> 'a M.t) option) (condition : bool)
      (a : 'a) ~(f : 'a -> 'a M.t) =
    map_when_m ?otherwise (not condition) ~f a

  let unless_m ?(otherwise : (unit -> unit M.t) option) (condition : bool)
      ~(f : unit -> unit M.t) : 'a M.t =
    map_unless_m ?otherwise condition ~f ()

  let tee_m (x : 'a) ~(f : 'a -> unit M.t) : 'a M.t = M.(f x >>| Fn.const x)

  let tee (x : 'a) ~(f : 'a -> unit) : 'a M.t = f x ; M.return x
end

module To_mappable (M : Monad.S) :
  Mappable_types.S1 with type 'a t := 'a M.t = struct
  let map = M.map
end

module S2_to_S (M : Monad.S2) (B : T) :
  Monad.S with type 'a t := ('a, B.t) M.t = Monad.Make (struct
  type 'a t = ('a, B.t) M.t

  let map = `Custom M.map

  let return = M.return

  let bind = M.bind
end)
