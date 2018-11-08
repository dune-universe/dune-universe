module Poly = struct
  let cmp x y =
    Compare.Poly.compare x y

  type 'a t = 'a BinaryHeap.t

  let empty = BinaryHeap.empty

  let add xs ~v =
    BinaryHeap.add xs ~cmp v

  let max xs =
    BinaryHeap.max xs

  let pop_max xs =
    BinaryHeap.pop_max ~cmp xs
end

module Make(E: Traits.Comparable.Basic.S0) = struct
  let cmp x y =
    E.compare x y

  type t = E.t BinaryHeap.t

  let empty = BinaryHeap.empty

  let add xs ~v =
    BinaryHeap.add xs ~cmp v

  let max xs =
    BinaryHeap.max xs

  let pop_max xs =
    BinaryHeap.pop_max ~cmp xs
end
