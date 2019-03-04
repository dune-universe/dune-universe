(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

include Bi_mappable_intf

module Extend0 (S : S0) : Extensions0
  with type t := S.t
   and type left := S.left
   and type right := S.right = struct
  let map_left  c ~f = S.bi_map c ~left:f ~right:Fn.id
  let map_right c ~f = S.bi_map c ~left:Fn.id ~right:f

  module Map_left = struct
    type elt = S.left
    let map = map_left
  end

  module Map_right = struct
    type elt = S.right
    let map = map_right
  end
end

module Extend1_left (S : S1_left) : Extensions1_left
  with type 'l t := 'l S.t
   and type right := S.right = struct
  let map_left  c ~f = S.bi_map c ~left:f ~right:Fn.id
  let map_right c ~f = S.bi_map c ~left:Fn.id ~right:f
  let map = map_left

  module Fix_left (Left : T) = struct
    module M = struct
      type t = Left.t S.t
      type left = Left.t
      type right = S.right
      let bi_map = S.bi_map
    end
    include M
    include Extend0 (M)
  end
end

module Extend1_right (S : S1_right) : Extensions1_right
  with type 'r t := 'r S.t
   and type left := S.left = struct
  let map_left  c ~f = S.bi_map c ~left:f ~right:Fn.id
  let map_right c ~f = S.bi_map c ~left:Fn.id ~right:f
  let map = map_right

  module Fix_right (Right : T) = struct
    module M = struct
      type t = Right.t S.t
      type left = S.left
      type right = Right.t
      let bi_map = S.bi_map
    end
    include M
    include Extend0 (M)
  end
end

module Extend2 (S : S2) : Extensions2
  with type ('l, 'r) t := ('l, 'r) S.t = struct
  let map_left  c ~f = S.bi_map c ~left:f ~right:Fn.id
  let map_right c ~f = S.bi_map c ~left:Fn.id ~right:f

  module Fix_left (Left : T) = struct
    module M = struct
      type 'r t = (Left.t, 'r) S.t
      type left = Left.t
      let bi_map = S.bi_map
    end
    include M
    include Extend1_right (M)
  end

  module Fix_right (Right : T) = struct
    module M = struct
      type 'l t = ('l, Right.t) S.t
      type right = Right.t
      let bi_map = S.bi_map
    end
    include M
    include Extend1_left (M)
  end
end
