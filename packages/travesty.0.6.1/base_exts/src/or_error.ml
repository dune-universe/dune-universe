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

open Travesty

module BT :
  Bi_traversable_types.S1_left
    with type 'l t = 'l Base.Or_error.t
     and type right = Base.Error.t =
  Bi_traversable.Fix2_right (Result) (Base.Error)

include BT

module On_ok : Traversable_types.S1 with type 'a t = 'a Base.Or_error.t =
  Bi_traversable.Traverse1_left (BT)

include Monad_exts.Extend (Base.Or_error)

let combine_map (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
  xs |> Base.List.map ~f |> Base.Or_error.combine_errors

let combine_map_unit (xs : 'a list) ~(f : 'a -> unit t) : unit t =
  xs |> Base.List.map ~f |> Base.Or_error.combine_errors_unit
