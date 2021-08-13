open Cps_toolbox
open Functional

let set_gen order item_gen st =
  let open QCheck.Gen in
  List.sort_unique order (list item_gen st) |> Set.from_list

let set_shrink items yield =
  QCheck.Shrink.list (Set.to_list items) (yield <== Set.from_list)

let set_print item_print set =
  let open Printf in
  let rec _print items return =
    match items with
    | [] -> return ""
    | item :: [] -> return (item_print item)
    | item :: items' ->
      _print items' (return <== (sprintf "%s, %s" (item_print item)))
  in
  _print (Set.to_list set) (sprintf "{%s}")

let arbitrary_set order a =
  QCheck.make (set_gen order a.gen)
    ?print:(Option.map set_print QCheck.(a.print))
    ~shrink:set_shrink

let set_is_subset order xs ys =
  Set.fold true
    (fun x prev ->
      if not prev then false else
      Set.is_member order x ys
        (fun () -> false)
        (fun () -> true))
    xs

let set_has_intersection order xs ys =
  Set.fold false
    (fun x prev ->
      if prev then true else
      Set.is_member order x ys
        (fun () -> false)
        (fun () -> true))
    xs

(* Define tests *)
let set_union_sound =
  QCheck.Test.make ~count:1000
    ~name:"set_union_sound"
    QCheck.(pair
      (arbitrary_set Order.int int)
      (arbitrary_set Order.int int))
    (fun (xs, ys) ->
      Set.union Order.int xs ys |> fun xys ->
      (set_is_subset Order.int xs xys) &&
      (set_is_subset Order.int ys xys))

let set_difference_sound =
  QCheck.Test.make ~count:1000
    ~name:"set_difference_sound"
    QCheck.(pair
      (arbitrary_set Order.int int)
      (arbitrary_set Order.int int))
    (fun (xs, ys) ->
      Set.difference Order.int xs ys |> fun zs ->
      (set_is_subset Order.int zs xs) &&
      not (set_has_intersection Order.int zs ys))

let set_intersection_sound =
  QCheck.Test.make ~count:1000
    ~name:"set_intersection_sound"
    QCheck.(pair
      (arbitrary_set Order.int int)
      (arbitrary_set Order.int int))
    (fun (xs, ys) ->
      Set.intersection Order.int xs ys |> fun zs ->
      (set_is_subset Order.int zs xs) &&
      (set_is_subset Order.int zs ys))

let set_has_intersection_sound =
  QCheck.Test.make ~count:1000
    ~name:"set_has_intersection_sound"
    QCheck.(pair
      (arbitrary_set Order.int int)
      (arbitrary_set Order.int int))
    (fun (xs, ys) ->
      Set.union Order.int xs ys |> fun zs ->
      match Set.is_empty xs, Set.is_empty ys with
      | true, true -> true
      | true, false ->
        Set.has_intersection Order.int zs ys
          (fun () -> false)
          (fun () -> true)
      | false, true | false, false ->
        Set.has_intersection Order.int zs xs
          (fun () -> false)
          (fun () -> true))

(* Expose tests *)
let test_suite =
  [ set_union_sound
  ; set_difference_sound
  ; set_intersection_sound
  ; set_has_intersection_sound
  ]
