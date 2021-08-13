open Cps_toolbox
open Functional

let avl_gen order item_gen st =
  let open QCheck in
  let open Gen in
  List.sort_unique order (list item_gen st) |> AVL.from_list

let avl_shrink tree yield =
  QCheck.Shrink.list (AVL.to_list tree) (yield <== AVL.from_list)

let avl_print item_print tree =
  let open Printf in
  let rec _print tree =
    match tree with
    | AVL.Null -> "E"
    | AVL.Node (_, _, d, Null, Null) -> sprintf "Z %s" (item_print d)
    | AVL.Node (c, h, d, Null, r) ->
      sprintf "R (%d, %d, %s, %s)" c h (item_print d) (_print r)
    | AVL.Node (c, h, d, l, Null) ->
      sprintf "L (%d, %d, %s, %s)" c h (item_print d) (_print l)
    | AVL.Node (c, h, d, l, r) ->
      sprintf "B (%d, %d, %s, %s, %s)" c h (item_print d) (_print l) (_print r)
  in
  _print tree

let arbitrary_avl order a =
  let open QCheck in
  make (avl_gen order a.gen)
    ?print:(Option.map avl_print a.print)
    ~shrink:avl_shrink

let compute_height =
  AVL.fold 0 (fun _ _ _ l r -> (max l r) + 1)

let height_is_height =
  QCheck.Test.make ~count:100
    ~name:"height_is_height"
    QCheck.(arbitrary_avl Order.int int)
    (fun tree ->
      (AVL.get_height tree) = (compute_height tree))

let compute_count =
  AVL.fold 0 (fun _ _ _ l r -> l + r + 1)

let count_is_count =
  QCheck.Test.make ~count:1000
    ~name:"count_is_count"
    QCheck.(arbitrary_avl Order.int int)
    (fun tree ->
      (AVL.get_count tree) = (compute_count tree))

let log2 n =
  if n <= 0 then 0 else
  let rec _visit m acc =
    if n <= m then acc else
    _visit (m lsl 1) (acc + 1)
  in
  _visit 1 1

let height_is_logarithmic =
  QCheck.Test.make ~count:1000
    ~name:"height_is_logarithmic"
    QCheck.(arbitrary_avl Order.int int)
    (fun tree ->
      (AVL.get_height tree) <= (log2 (AVL.get_count tree)))

let list_is_ordered order xs =
  let open Order in
  let rec _visit y xs =
    match xs with
    | [] -> true
    | x :: xs' ->
      match order y x with
      | LT | EQ -> _visit x xs'
      | GT -> false
  in
  match xs with
  | [] -> true
  | x :: xs' ->
    _visit x xs'

let items_are_ordered =
  QCheck.Test.make ~count:1000
    ~name:"items_are_ordered"
    QCheck.(arbitrary_avl Order.int int)
    (fun tree ->
      list_is_ordered Order.int (AVL.to_list tree))

let from_to_list_involutive =
  QCheck.Test.make ~count:1000
    ~name:"from_to_list_involutive"
    QCheck.(list int)
    (fun items ->
      List.sort_unique Order.int items |> fun items1 ->
      AVL.to_list (AVL.from_list items1) = items1)

let from_list_is_logarithmic =
  QCheck.Test.make ~count:1000
    ~name:"from_list_is_logarithmic"
    QCheck.(list int)
    (fun items ->
      List.sort_unique Order.int items |> fun items1 ->
      AVL.from_list items1 |> fun tree ->
      (AVL.get_height tree) <= (log2 (List.length items1)))

(* Expose tests *)
let test_suite =
  [ height_is_height
  ; count_is_count
  ; height_is_logarithmic
  ; items_are_ordered
  ; from_to_list_involutive
  ; from_list_is_logarithmic
  ]
