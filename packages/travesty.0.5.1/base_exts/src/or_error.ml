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

type 'a t = 'a Or_error.t

module On_ok = Traversable.Make1 (struct
  type nonrec 'a t = 'a t

  module On_monad (M : Base.Monad.S) = struct
    let map_m err ~f =
      match err with
      | Ok v ->
          M.(f v >>| Base.Or_error.return)
      | Error x ->
          M.return (Error x)
  end
end)

module BM = Bi_mappable.Make1_left (struct
  type 'l t = 'l Or_error.t

  type right = Error.t

  let bi_map (e : 'l1 Or_error.t) ~(left : 'l1 -> 'l2)
      ~(right : Error.t -> Error.t) : 'l2 Or_error.t =
    e |> Result.map_error ~f:right |> Result.map ~f:left
end)

include (BM : module type of BM with type 'l t := 'l t)

include Monad_exts.Extend (Base.Or_error)

let combine_map (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
  xs |> Base.List.map ~f |> Base.Or_error.combine_errors

let combine_map_unit (xs : 'a list) ~(f : 'a -> unit t) : unit t =
  xs |> Base.List.map ~f |> Base.Or_error.combine_errors_unit
