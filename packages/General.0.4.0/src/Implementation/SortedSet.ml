module Tree = RedBlackTree

module Poly = struct
  let cmp = Compare.Poly.compare
  let cmp_k = cmp

  (* @todo Forbid using Poly.equal and Poly.compare *)
  type 'a t = 'a Tree.t

  let empty = Tree.empty

  let is_empty = Tree.is_empty

  let size t =
    Tree.size t ~cmp

  let add t ~v =
    Tree.add t ~cmp v

  let of_list vs =
    vs
    |> List.fold ~init:empty ~f:(fun t v ->
      add t ~v
      |> Tuples.Tuple2.get_1
    )

  let replace t ~v =
    Tree.replace t ~cmp v

  let remove t ~v =
    Tree.remove t ~cmp ~cmp_k v

  let to_list t =
    Tree.to_list t ~cmp

  let contains t ~v =
    Tree.try_get t ~cmp ~cmp_k v
    |> Option.is_some
end

module Make(E: Traits.Comparable.Basic.S0) = struct
  let cmp = E.compare
  let cmp_k = cmp

  (* @todo Forbid using Poly.equal and Poly.compare *)
  type t = E.t Tree.t

  let empty = Tree.empty

  let is_empty = Tree.is_empty

  let size t =
    Tree.size t ~cmp

  let add t ~v =
    Tree.add t ~cmp v

  let of_list vs =
    vs
    |> List.fold ~init:empty ~f:(fun t v ->
      add t ~v
      |> Tuples.Tuple2.get_1
    )

  let replace t ~v =
    Tree.replace t ~cmp v

  let remove t ~v =
    Tree.remove t ~cmp ~cmp_k v

  let to_list t =
    Tree.to_list t ~cmp

  let contains t ~v =
    Tree.try_get t ~cmp ~cmp_k v
    |> Option.is_some
end
