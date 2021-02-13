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
open Bi_traversable_types

(** [Derived_ops_maker] is an internal module type used for implementing the
    derived operations (map-left, map-right) in an arity-generic way. *)
module type Derived_ops_maker = sig
  include Generic_types.Bi_generic

  module On (M : Applicative.S) :
    Basic_generic_on_applicative
      with module M := M
       and type ('l, 'r) t := ('l, 'r) t
       and type 'l left := 'l left
       and type 'r right := 'r right
end

(** [Derived_ops_applicative_gen] is an internal functor used to generate
    several derived operations from an applicative bi-traversal in an
    arity-generic way. *)
module Derived_ops_applicative_gen
    (I : Derived_ops_maker)
    (M : Applicative.S) =
struct
  module IM = I.On (M)

  let map_left_m (c : ('l1, 'r) I.t) ~(f : 'l1 I.left -> 'l2 I.left M.t) :
      ('l2, 'r) I.t M.t =
    IM.bi_map_m c ~left:f ~right:M.return

  let map_right_m (c : ('l, 'r1) I.t) ~(f : 'r1 I.right -> 'r2 I.right M.t) :
      ('l, 'r2) I.t M.t =
    IM.bi_map_m c ~left:M.return ~right:f
end

module Make0 (I : Basic0) :
  S0 with type t = I.t and type left = I.left and type right = I.right =
struct
  module On (MS : Applicative.S) = struct
    include I.On (MS)

    include
      Derived_ops_applicative_gen
        (struct
          type ('l, 'r) t = I.t

          type 'l left = I.left

          type 'r right = I.right

          module On = I.On
        end)
        (MS)
  end

  module On_monad (M : Monad.S) = On (Monad_exts.App (M))
  module With_errors = On_monad (Or_error)
  module Ident = On_monad (Monad.Ident)

  include Bi_mappable.Make0 (struct
    type t = I.t

    type left = I.left

    type right = I.right

    let bi_map = Ident.bi_map_m
  end)
end

module Make1_left (I : Basic1_left) :
  S1_left with type 'l t = 'l I.t and type right = I.right = struct
  module On (MS : Applicative.S) = struct
    include I.On (MS)

    include
      Derived_ops_applicative_gen
        (struct
          type ('l, 'r) t = 'l I.t

          type 'l left = 'l

          type 'r right = I.right

          module On = I.On
        end)
        (MS)
  end

  module On_monad (M : Monad.S) = On (Monad_exts.App (M))
  module With_errors = On_monad (Or_error)
  module Ident = On_monad (Monad.Ident)

  include Bi_mappable.Make1_left (struct
    type 'l t = 'l I.t

    type right = I.right

    let bi_map = Ident.bi_map_m
  end)
end

module Make1_right (I : Basic1_right) :
  S1_right with type 'r t = 'r I.t and type left = I.left = struct
  module On (MS : Applicative.S) = struct
    include I.On (MS)

    include
      Derived_ops_applicative_gen
        (struct
          type ('l, 'r) t = 'r I.t

          type 'l left = I.left

          type 'r right = 'r

          module On = I.On
        end)
        (MS)
  end

  module On_monad (M : Monad.S) = On (Monad_exts.App (M))
  module With_errors = On_monad (Or_error)
  module Ident = On_monad (Monad.Ident)

  include Bi_mappable.Make1_right (struct
    type 'r t = 'r I.t

    type left = I.left

    let bi_map = Ident.bi_map_m
  end)
end

module Make2 (I : Basic2) : S2 with type ('l, 'r) t = ('l, 'r) I.t = struct
  module On (MS : Applicative.S) = struct
    include I.On (MS)

    include
      Derived_ops_applicative_gen
        (struct
          type ('l, 'r) t = ('l, 'r) I.t

          type 'l left = 'l

          type 'r right = 'r

          module On = I.On
        end)
        (MS)
  end

  module On_monad (M : Monad.S) = On (Monad_exts.App (M))
  module With_errors = On_monad (Or_error)
  module Ident = On_monad (Monad.Ident)

  include Bi_mappable.Make2 (struct
    type ('l, 'r) t = ('l, 'r) I.t

    let bi_map = Ident.bi_map_m
  end)
end

module Fix2_left (I : Basic2) (Left : T) :
  S1_right with type 'r t = (Left.t, 'r) I.t and type left = Left.t =
Make1_right (struct
  type 'r t = (Left.t, 'r) I.t

  type left = Left.t

  module On = I.On
end)

module Fix2_right (I : Basic2) (Right : T) :
  S1_left with type 'l t = ('l, Right.t) I.t and type right = Right.t =
Make1_left (struct
  type 'l t = ('l, Right.t) I.t

  type right = Right.t

  module On = I.On
end)

module Fix2_both (I : Basic2) (Left : T) (Right : T) :
  S0
    with type t = (Left.t, Right.t) I.t
     and type left = Left.t
     and type right = Right.t = Make0 (struct
  type t = (Left.t, Right.t) I.t

  type left = Left.t

  type right = Right.t

  module On = I.On
end)

module Fix1_left (I : Basic1_left) (Left : T) :
  S0 with type t = Left.t I.t and type left = Left.t and type right = I.right =
Make0 (struct
  type t = Left.t I.t

  type left = Left.t

  type right = I.right

  module On = I.On
end)

module Fix1_right (I : Basic1_right) (Right : T) :
  S0
    with type t = Right.t I.t
     and type left = I.left
     and type right = Right.t = Make0 (struct
  type t = Right.t I.t

  type left = I.left

  type right = Right.t

  module On = I.On
end)

module Traverse1_left (I : S1_left) :
  Traversable_types.S1 with type 'l t = 'l I.t = Traversable.Make1 (struct
  type 'l t = 'l I.t

  module On (M : Applicative.S) = struct
    module IM = I.On (M)

    let map_m = IM.map_left_m
  end
end)

module Traverse1_right (I : S1_right) :
  Traversable_types.S1 with type 'r t = 'r I.t = Traversable.Make1 (struct
  type 'r t = 'r I.t

  module On (M : Applicative.S) = struct
    module IM = I.On (M)

    let map_m = IM.map_right_m
  end
end)

module Traverse0_left (L : Equal.S) (I : S0 with type left := L.t) :
  Traversable_types.S0 with type t = I.t and module Elt = L =
Traversable.Make0 (struct
  type t = I.t

  module Elt = L

  module On (M : Applicative.S) = struct
    module IM = I.On (M)

    let map_m = IM.map_left_m
  end
end)

module Traverse0_right (R : Equal.S) (I : S0 with type right := R.t) :
  Traversable_types.S0 with type t = I.t and module Elt = R =
Traversable.Make0 (struct
  type t = I.t

  module Elt = R

  module On (M : Applicative.S) = struct
    module IM = I.On (M)

    let map_m = IM.map_right_m
  end
end)

module Chain_Bi2_Traverse1 (Bi : Basic2) (Trav : Traversable_types.Basic1) :
  S2 with type ('l, 'r) t = ('l, 'r) Bi.t Trav.t = Make2 (struct
  type ('l, 'r) t = ('l, 'r) Bi.t Trav.t

  module On (M : Applicative.S) = struct
    module MBi = Bi.On (M)
    module MTrav = Trav.On (M)

    let bi_map_m (x : ('l1, 'r1) t) ~(left : 'l1 -> 'l2 M.t)
        ~(right : 'r1 -> 'r2 M.t) : ('l2, 'r2) t M.t =
      MTrav.map_m x ~f:(MBi.bi_map_m ~left ~right)
  end
end)

module Chain_Bi1_left_Traverse1
    (Bi : Basic1_left)
    (Trav : Traversable_types.Basic1) :
  S1_left with type 'l t = 'l Bi.t Trav.t and type right = Bi.right =
Make1_left (struct
  type 'l t = 'l Bi.t Trav.t

  type right = Bi.right

  module On (M : Applicative.S) = struct
    module MBi = Bi.On (M)
    module MTrav = Trav.On (M)

    let bi_map_m (x : 'l1 t) ~(left : 'l1 -> 'l2 M.t)
        ~(right : right -> right M.t) : 'l2 t M.t =
      MTrav.map_m x ~f:(MBi.bi_map_m ~left ~right)
  end
end)

module Chain_Bi1_right_Traverse1
    (Bi : Basic1_right)
    (Trav : Traversable_types.Basic1) :
  S1_right with type 'r t = 'r Bi.t Trav.t and type left = Bi.left =
Make1_right (struct
  type 'r t = 'r Bi.t Trav.t

  type left = Bi.left

  module On (M : Applicative.S) = struct
    module MBi = Bi.On (M)
    module MTrav = Trav.On (M)

    let bi_map_m (x : 'r1 t) ~(left : left -> left M.t)
        ~(right : 'r1 -> 'r2 M.t) : 'r2 t M.t =
      MTrav.map_m x ~f:(MBi.bi_map_m ~left ~right)
  end
end)

module Chain_Bi0_Traverse1 (Bi : Basic0) (Trav : Traversable_types.Basic1) :
  S0
    with type t = Bi.t Trav.t
     and type left = Bi.left
     and type right = Bi.right = Make0 (struct
  type t = Bi.t Trav.t

  type left = Bi.left

  type right = Bi.right

  module On (M : Applicative.S) = struct
    module MBi = Bi.On (M)
    module MTrav = Trav.On (M)

    let bi_map_m (x : t) ~(left : left -> left M.t)
        ~(right : right -> right M.t) : t M.t =
      MTrav.map_m x ~f:(MBi.bi_map_m ~left ~right)
  end
end)

module Chain_Traverse1_Bi2
    (LTrav : Traversable_types.Basic1)
    (RTrav : Traversable_types.Basic1)
    (Bi : Basic2) : S2 with type ('l, 'r) t = ('l LTrav.t, 'r RTrav.t) Bi.t =
Make2 (struct
  type ('l, 'r) t = ('l LTrav.t, 'r RTrav.t) Bi.t

  module On (M : Applicative.S) = struct
    module MBi = Bi.On (M)
    module MTrav1 = LTrav.On (M)
    module MTrav2 = RTrav.On (M)

    let bi_map_m (x : ('l1, 'r1) t) ~(left : 'l1 -> 'l2 M.t)
        ~(right : 'r1 -> 'r2 M.t) : ('l2, 'r2) t M.t =
      MBi.bi_map_m x ~left:(MTrav1.map_m ~f:left)
        ~right:(MTrav2.map_m ~f:right)
  end
end)

module Chain_Traverse1_Bi1_left
    (LTrav : Traversable_types.Basic1)
    (Bi : Basic1_left) :
  S1_left with type 'l t = 'l LTrav.t Bi.t and type right = Bi.right =
Make1_left (struct
  type 'l t = 'l LTrav.t Bi.t

  type right = Bi.right

  module On (M : Applicative.S) = struct
    module MBi = Bi.On (M)
    module MTrav = LTrav.On (M)

    let bi_map_m (x : 'l1 t) ~(left : 'l1 -> 'l2 M.t)
        ~(right : right -> right M.t) : 'l2 t M.t =
      MBi.bi_map_m x ~left:(MTrav.map_m ~f:left) ~right
  end
end)

module Chain_Traverse1_Bi1_right
    (RTrav : Traversable_types.Basic1)
    (Bi : Basic1_right) :
  S1_right with type 'r t = 'r RTrav.t Bi.t and type left = Bi.left =
Make1_right (struct
  type 'r t = 'r RTrav.t Bi.t

  type left = Bi.left

  module On (M : Applicative.S) = struct
    module MBi = Bi.On (M)
    module MTrav = RTrav.On (M)

    let bi_map_m (x : 'r1 t) ~(left : left -> left M.t)
        ~(right : 'r1 -> 'r2 M.t) : 'r2 t M.t =
      MBi.bi_map_m x ~left ~right:(MTrav.map_m ~f:right)
  end
end)
