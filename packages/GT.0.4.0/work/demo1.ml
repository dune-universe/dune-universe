type 'a list = 'a List.t = [] | ( :: ) of 'a * 'a list [@@deriving fold]

type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree [@@deriving fold]

module Manual = struct
  let rec sum_list = function [] -> 0 | n :: xs -> n + sum_list xs

  let%test _ = sum_list [ 1; 2; 3 ] = 6

  let%test _ = sum_list [ 1; 2; 3 ] = 6

  let rec sum_btree = function
    | Leaf n -> n
    | Node (l, r) -> sum_btree l + sum_btree r

  let%test _ = sum_btree (Node (Node (Leaf 1, Leaf 2), Leaf 3)) = 6

  let rec fold_btree_left f acc = function
    | Leaf _ -> f acc 1
    | Node (l, r) -> fold_btree_left f (fold_btree_left f acc l) r

  let sum_btree2 t = fold_btree_left (fun x y -> 1 + x + y) 0 t

  let%test _ =
    let s = sum_btree2 (Node (Node (Leaf 1, Leaf 2), Leaf 3)) in
    s = 6
end

let sum_list2 xs = fold_list ( + ) 0 xs

let%test _ = sum_list2 [ 1; 2; 3 ] = 6

let sum_btree2 t = fold_btree ( + ) 0 t

let () = ()

let%test _ = sum_btree2 (Node (Node (Leaf 1, Leaf 2), Leaf 3)) = 6
