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

open Base
open Travesty

module BT :
  Bi_traversable_types.S2 with type ('l, 'r) t = ('l, 'r) Result.t =
Bi_traversable.Make2 (struct
  type ('l, 'r) t = ('l, 'r) Result.t

  module On_monad (M : Monad.S) = struct
    let bi_map_m (e : ('l1, 'r1) Result.t) ~(left : 'l1 -> 'l2 M.t)
        ~(right : 'r1 -> 'r2 M.t) : ('l2, 'r2) Result.t M.t =
      match e with
      | Ok x ->
          M.(left x >>| Result.return)
      | Error y ->
          M.(right y >>| Result.fail)
  end
end)

include BT
