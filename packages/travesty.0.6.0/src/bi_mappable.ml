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
open Bi_mappable_types

module Derived_ops_gen (I : Basic_generic) = struct
  let map_left (c : ('l1, 'r) I.t) ~(f : 'l1 I.left -> 'l2 I.left) :
      ('l2, 'r) I.t =
    I.bi_map c ~left:f ~right:Fn.id

  let map_right (c : ('l, 'r1) I.t) ~(f : 'r1 I.right -> 'r2 I.right) :
      ('l, 'r2) I.t =
    I.bi_map c ~left:Fn.id ~right:f
end

module Make0 (I : Basic0) :
  S0 with type t = I.t and type left = I.left and type right = I.right =
struct
  include I

  include Derived_ops_gen (struct
    type ('l, 'r) t = I.t

    type 'l left = I.left

    type 'r right = I.right

    let bi_map = I.bi_map
  end)
end

module Make1_left (I : Basic1_left) :
  S1_left with type 'l t = 'l I.t and type right = I.right = struct
  include I

  include Derived_ops_gen (struct
    type ('l, 'r) t = 'l I.t

    type 'l left = 'l

    type 'r right = I.right

    let bi_map = I.bi_map
  end)
end

module Make1_right (I : Basic1_right) :
  S1_right with type 'r t = 'r I.t and type left = I.left = struct
  include I

  include Derived_ops_gen (struct
    type ('l, 'r) t = 'r I.t

    type 'l left = I.left

    type 'r right = 'r

    let bi_map = I.bi_map
  end)
end

module Make2 (I : Basic2) : S2 with type ('l, 'r) t = ('l, 'r) I.t = struct
  include I

  include Derived_ops_gen (struct
    type ('l, 'r) t = ('l, 'r) I.t

    type 'l left = 'l

    type 'r right = 'r

    let bi_map = I.bi_map
  end)
end

module Fix2_left (I : Basic2) (Left : T) :
  S1_right with type 'r t = (Left.t, 'r) I.t and type left = Left.t =
Make1_right (struct
  type 'r t = (Left.t, 'r) I.t

  type left = Left.t

  let bi_map = I.bi_map
end)

module Fix2_right (I : Basic2) (Right : T) :
  S1_left with type 'l t = ('l, Right.t) I.t and type right = Right.t =
Make1_left (struct
  type 'l t = ('l, Right.t) I.t

  type right = Right.t

  let bi_map = I.bi_map
end)

module Fix2_both (I : Basic2) (Left : T) (Right : T) :
  S0
  with type t = (Left.t, Right.t) I.t
   and type left = Left.t
   and type right = Right.t = Make0 (struct
  type t = (Left.t, Right.t) I.t

  type left = Left.t

  type right = Right.t

  let bi_map = I.bi_map
end)

module Fix1_left (I : Basic1_left) (Left : T) :
  S0
  with type t = Left.t I.t
   and type left = Left.t
   and type right = I.right = Make0 (struct
  type t = Left.t I.t

  type left = Left.t

  type right = I.right

  let bi_map = I.bi_map
end)

module Fix1_right (I : Basic1_right) (Right : T) :
  S0
  with type t = Right.t I.t
   and type left = I.left
   and type right = Right.t = Make0 (struct
  type t = Right.t I.t

  type left = I.left

  type right = Right.t

  let bi_map = I.bi_map
end)

module Map1_left (I : S1_left) : Mappable_types.S1 with type 'l t = 'l I.t =
struct
  type 'l t = 'l I.t

  let map = I.map_left
end

module Map1_right (I : S1_right) :
  Mappable_types.S1 with type 'r t = 'r I.t = struct
  type 'r t = 'r I.t

  let map = I.map_right
end

module Map0_left (I : S0) :
  Mappable_types.S0 with type t = I.t and type elt = I.left = struct
  type t = I.t

  type elt = I.left

  let map = I.map_left
end

module Map0_right (I : S0) :
  Mappable_types.S0 with type t = I.t and type elt = I.right = struct
  type t = I.t

  type elt = I.right

  let map = I.map_right
end

module Chain_Bi2_Map1 (Bi : Basic2) (Map : Mappable_types.S1) :
  S2 with type ('l, 'r) t = ('l, 'r) Bi.t Map.t = Make2 (struct
  type ('l, 'r) t = ('l, 'r) Bi.t Map.t

  let bi_map (x : ('l1, 'r1) t) ~(left : 'l1 -> 'l2) ~(right : 'r1 -> 'r2) :
      ('l2, 'r2) t =
    Map.map x ~f:(Bi.bi_map ~left ~right)
end)

module Chain_Bi1_left_Map1 (Bi : Basic1_left) (Map : Mappable_types.S1) :
  S1_left with type 'l t = 'l Bi.t Map.t and type right = Bi.right =
Make1_left (struct
  type 'l t = 'l Bi.t Map.t

  type right = Bi.right

  let bi_map (x : 'l1 t) ~(left : 'l1 -> 'l2) ~(right : right -> right) :
      'l2 t =
    Map.map x ~f:(Bi.bi_map ~left ~right)
end)

module Chain_Bi1_right_Map1 (Bi : Basic1_right) (Map : Mappable_types.S1) :
  S1_right with type 'r t = 'r Bi.t Map.t and type left = Bi.left =
Make1_right (struct
  type 'r t = 'r Bi.t Map.t

  type left = Bi.left

  let bi_map (x : 'r1 t) ~(left : left -> left) ~(right : 'r1 -> 'r2) :
      'r2 t =
    Map.map x ~f:(Bi.bi_map ~left ~right)
end)

module Chain_Bi0_Map1 (Bi : Basic0) (Map : Mappable_types.S1) :
  S0
  with type t = Bi.t Map.t
   and type left = Bi.left
   and type right = Bi.right = Make0 (struct
  type t = Bi.t Map.t

  type left = Bi.left

  type right = Bi.right

  let bi_map (x : t) ~(left : left -> left) ~(right : right -> right) : t =
    Map.map x ~f:(Bi.bi_map ~left ~right)
end)

module Chain_Map1_Bi2
    (LMap : Mappable_types.S1)
    (RMap : Mappable_types.S1)
    (Bi : Basic2) : S2 with type ('l, 'r) t = ('l LMap.t, 'r RMap.t) Bi.t =
Make2 (struct
  type ('l, 'r) t = ('l LMap.t, 'r RMap.t) Bi.t

  let bi_map (x : ('l1, 'r1) t) ~(left : 'l1 -> 'l2) ~(right : 'r1 -> 'r2) :
      ('l2, 'r2) t =
    Bi.bi_map x ~left:(LMap.map ~f:left) ~right:(RMap.map ~f:right)
end)

module Chain_Map1_Bi1_left (LMap : Mappable_types.S1) (Bi : Basic1_left) :
  S1_left with type 'l t = 'l LMap.t Bi.t and type right = Bi.right =
Make1_left (struct
  type 'l t = 'l LMap.t Bi.t

  type right = Bi.right

  let bi_map (x : 'l1 t) ~(left : 'l1 -> 'l2) ~(right : right -> right) :
      'l2 t =
    Bi.bi_map x ~left:(LMap.map ~f:left) ~right
end)

module Chain_Map1_Bi1_right (RMap : Mappable_types.S1) (Bi : Basic1_right) :
  S1_right with type 'r t = 'r RMap.t Bi.t and type left = Bi.left =
Make1_right (struct
  type 'r t = 'r RMap.t Bi.t

  type left = Bi.left

  let bi_map (x : 'r1 t) ~(left : left -> left) ~(right : 'r1 -> 'r2) :
      'r2 t =
    Bi.bi_map x ~left ~right:(RMap.map ~f:right)
end)
