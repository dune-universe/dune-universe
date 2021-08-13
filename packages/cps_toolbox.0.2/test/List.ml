open Cps_toolbox
open Functional

let _gen_iota count =
  let open QCheck.Gen in
  Nat.fold
    (fun _index return -> return [])
    (fun visit_pred index return ->
      visit_pred (index + 1) (return <== (List.cons index)))
    count 0 @@ fun xs ->
  shuffle_l xs >>= fun xs1 ->
  return (xs1, count)

let gen_iota =
  let open QCheck.Gen in
  nat >>= fun count ->
  _gen_iota count

let print_iota =
  QCheck.Print.(pair (list int) int)

let shrink_iota (_xs, count) =
  let open QCheck.Iter in
  QCheck.Shrink.int count >|= fun count1 ->
  QCheck.Gen.generate1 (_gen_iota count1)

let arbitrary_iota =
  QCheck.make gen_iota
    ~print: print_iota
    ~shrink: shrink_iota

let list_is_ordered order xs =
  let open Order in
  let rec _visit y xs =
    match xs with
    | [] -> true
    | x :: xs1 ->
      match order y x with
      | LT | EQ -> _visit x xs1
      | GT -> false
  in
  match xs with
  | [] -> true
  | x :: xs1 ->
    _visit x xs1

let list_is_ordered_unique order xs =
  let open Order in
  let rec _visit y xs =
    match xs with
    | [] -> true
    | x :: xs1 ->
      match order y x with
      | EQ | GT -> false
      | LT -> _visit x xs1
  in
  match xs with
  | [] -> true
  | x :: xs1 ->
    _visit x xs1

(* Define tests *)
let list_select_sound =
  QCheck.Test.make ~count:64
    ~name:"list_select_sound"
    arbitrary_iota
    (fun (xs, count) ->
      let open QCheck.Gen in
      if count = 0 then true else
      let n = (QCheck.Gen.generate1 (1 -- count)) - 1 in
      List.select Order.int n xs |> fun m ->
      n = m)

let list_sort_sound =
  QCheck.Test.make ~count:64
    ~name:"list_sort_sound"
    QCheck.(list int)
    (fun xs ->
      List.sort Order.int xs |> fun xs1 ->
      list_is_ordered Order.int xs1)

let list_sort_unique_sound =
  QCheck.Test.make ~count:64
    ~name:"list_sort_unique_sound"
    QCheck.(list int)
    (fun xs ->
      List.sort_unique Order.int xs |> fun xs1 ->
      list_is_ordered_unique Order.int xs1)

(* Expose tests *)
let test_suite =
  [ list_select_sound
  ; list_sort_sound
  ; list_sort_unique_sound
  ]
