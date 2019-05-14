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

open Base
include Filter_mappable_intf

module Make_generic (F : Generic_basic) :
  Generic with type 'a t := 'a F.t and type 'a elt := 'a F.elt = struct
  include F

  let filter xs ~f = filter_map xs ~f:(fun x -> Option.some_if (f x) x)

  let exclude xs ~f = filter xs ~f:(Fn.non f)
end

module Make0 (F : Basic0) : S0 with type t := F.t and type elt := F.elt =
Make_generic (struct
  type 'a t = F.t

  type 'a elt = F.elt

  let filter_map = F.filter_map
end)

module Make1 (F : Basic1) : S1 with type 'a t := 'a F.t =
Make_generic (struct
  type 'a t = 'a F.t

  type 'a elt = 'a

  let filter_map = F.filter_map
end)

module To_mappable_generic (F : Generic_basic) :
  Mappable.Generic with type 'a t := 'a F.t and type 'a elt := 'a F.elt =
struct
  include F

  let map xs ~f = filter_map xs ~f:(fun x -> Some (f x))
end

module To_mappable0 (F : Basic0) :
  Mappable.S0 with type t := F.t and type elt := F.elt =
To_mappable_generic (struct
  type 'a t = F.t

  type 'a elt = F.elt

  let filter_map = F.filter_map
end)

module To_mappable1 (F : Basic1) : Mappable.S1 with type 'a t := 'a F.t =
To_mappable_generic (struct
  type 'a t = 'a F.t

  type 'a elt = 'a

  let filter_map = F.filter_map
end)
