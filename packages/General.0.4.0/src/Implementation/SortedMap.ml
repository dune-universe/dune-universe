module Tree = RedBlackTree

module Poly = struct
  let cmp (x, _) (y, _) =
    Compare.Poly.compare x y

  let cmp_k x (y, _) =
    Compare.Poly.compare x y

  type ('a, 'b) t = ('a * 'b) Tree.t

  let empty = Tree.empty

  let is_empty = Tree.is_empty

  let size t =
    Tree.size t ~cmp

  let add t ~k ~v =
    Tree.add t ~cmp (k, v)

  let of_list_first vs =
    vs
    |> List.fold ~init:empty ~f:(fun t (k, v) ->
      add t ~k ~v
      |> Tuples.Tuple2.get_1
    )

  let replace t ~k ~v =
    Tree.replace t ~cmp (k, v)

  let of_list_last vs =
    vs
    |> List.fold ~init:empty ~f:(fun t (k, v) ->
      replace t ~k ~v
    )

  let remove t ~k =
    Tree.remove t ~cmp ~cmp_k k

  let to_list t =
    Tree.to_list t ~cmp

  let try_get t ~k =
    Tree.try_get t ~cmp ~cmp_k k
    |> Option.map ~f:Tuples.Tuple2.get_1

  let get t ~k =
    try_get t ~k
    |> Option.or_failure ""
end

module Make(K: Traits.Comparable.Basic.S0) = struct
  let cmp (x, _) (y, _) =
    K.compare x y

  let cmp_k x (y, _) =
    K.compare x y

  type 'a t = (K.t * 'a) Tree.t

  let empty = Tree.empty

  let is_empty = Tree.is_empty

  let size t =
    Tree.size t ~cmp

  let add t ~k ~v =
    Tree.add t ~cmp (k, v)

  let of_list_first vs =
    vs
    |> List.fold ~init:empty ~f:(fun t (k, v) ->
      add t ~k ~v
      |> Tuples.Tuple2.get_1
    )

  let replace t ~k ~v =
    Tree.replace t ~cmp (k, v)

  let of_list_last vs =
    vs
    |> List.fold ~init:empty ~f:(fun t (k, v) ->
      replace t ~k ~v
    )

  let remove t ~k =
    Tree.remove t ~cmp ~cmp_k k

  let to_list t =
    Tree.to_list t ~cmp

  let try_get t ~k =
    Tree.try_get t ~cmp ~cmp_k k
    |> Option.map ~f:Tuples.Tuple2.get_1

  let get t ~k =
    try_get t ~k
    |> Option.or_failure ""
end
