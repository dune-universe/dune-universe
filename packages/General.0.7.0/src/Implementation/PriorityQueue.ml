module Poly = struct
  let cmp (x, _) (y, _) =
    Compare.Poly.compare x y

  type ('a, 'b) t = ('a * 'b) BinaryHeap.t

  let empty = BinaryHeap.empty

  let add xs ~k ~v =
    BinaryHeap.add xs ~cmp (k, v)

  let max xs =
    BinaryHeap.max xs

  let pop_max xs =
    BinaryHeap.pop_max ~cmp xs
end

module Make(E: Traits.Comparable.Basic.S0) = struct
  let cmp (x, _) (y, _) =
    E.compare x y

  type 'a t = (E.t * 'a) BinaryHeap.t

  let empty = BinaryHeap.empty

  let add xs ~k ~v =
    BinaryHeap.add xs ~cmp (k, v)

  let max xs =
    BinaryHeap.max xs

  let pop_max xs =
    BinaryHeap.pop_max ~cmp xs
end
