open Functional

let nil = []
let cons x xs = x :: xs

let fold null_case list_case term =
  let rec _visit xs return =
    match xs with
    | [] -> return null_case
    | x :: xs' ->
      _visit xs' (return <== (list_case x))
  in
  _visit term identity

let fold_rev null_case list_case term =
  let rec _visit xs result =
    match xs with
    | [] -> result
    | x :: xs' ->
      _visit xs' (list_case x result)
  in
  _visit term null_case

let length xs =
  fold_rev Nat.zero (fun _x -> Nat.succ) xs

let iter f xs =
  fold () (fun x () -> f x) xs

let init count item =
  if count <= 0 then [] else
  let rec _visit index result =
    if index = count then result else
    _visit (index + 1) (item :: result)
  in
  _visit 0 []

let map f xs =
  fold [] (cons <== f) xs

let conc xs ys =
  fold ys cons xs

let flatten xs =
  fold [] conc xs

let rec zip xs ys fail return =
  match xs, ys with
  | [], [] -> return []
  | x :: xs1, y :: ys1 ->
    zip xs1 ys1 fail @@ fun xys ->
    return ((x, y) :: xys)
  | _, _ -> fail ()

let select order n xs =
  let _ordered x1 x2 =
    let open Order in
    match order x1 x2 with
    | LT | EQ -> true
    | GT -> false
  in
  let _sort_2 x1 x2 return =
    if _ordered x1 x2
    then return x1 x2
    else return x2 x1
  in
  let _sort_3 x1 x2 x3 return =
    _sort_2 x1 x2 @@ fun x4 x5 ->
    if _ordered x3 x4 then return x3 x4 x5 else
    if _ordered x3 x5 then return x4 x3 x5 else
    return x4 x5 x3
  in
  let _sort_4 x1 x2 x3 x4 return =
    _sort_3 x1 x2 x3 @@ fun x5 x6 x7 ->
    if _ordered x4 x5 then return x4 x5 x6 x7 else
    if _ordered x4 x6 then return x5 x4 x6 x7 else
    if _ordered x4 x7 then return x5 x6 x4 x7 else
    return x5 x6 x7 x4
  in
  let _sort_5 x1 x2 x3 x4 x5 return =
    _sort_4 x1 x2 x3 x4 @@ fun x6 x7 x8 x9 ->
    if _ordered x7 x5 then
      if _ordered x5 x8 then return x6 x7 x5 x8 x9 else
      if _ordered x5 x9 then return x6 x7 x8 x5 x9 else
      return x6 x7 x8 x9 x5
    else
      if _ordered x6 x5
      then return x6 x5 x7 x8 x9
      else return x5 x6 x7 x8 x9
  in
  let rec _median_of_medians xs return =
    let rec _median_of_fives xs return =
      match xs with
      | [] -> assert false (* Invariant *)
      | x1 :: [] -> return [ x1 ]
      | x1 :: x2 :: [] ->
        _sort_2 x1 x2 @@ fun _x3 x4 -> return [ x4 ]
      | x1 :: x2 :: x3 :: [] ->
        _sort_3 x1 x2 x3 @@ fun _x4 x5 _x6 -> return [ x5 ]
      | x1 :: x2 :: x3 :: x4 :: [] ->
        _sort_4 x1 x2 x3 x4 @@ fun _x5 x6 _x7 _x8 -> return [ x6 ]
      | x1 :: x2 :: x3 :: x4 :: x5 :: [] ->
        _sort_5 x1 x2 x3 x4 x5 @@ fun _x6 _x7 x8 _x9 _x10 -> return [ x8 ]
      | x1 :: x2 :: x3 :: x4 :: x5 :: xs1 ->
        _sort_5 x1 x2 x3 x4 x5 @@ fun _x6 _x7 x8 _x9 _x10 ->
        _median_of_fives xs1 (return <== (cons x8))
    in
    match xs with
    | [] -> assert false (* Invariant *)
    | x1 :: [] -> return x1
    | x1 :: x2 :: [] ->
      _sort_2 x1 x2 @@ fun x3 _x4 -> return x3
    | x1 :: x2 :: x3 :: [] ->
      _sort_3 x1 x2 x3 @@ fun _x4 x5 _x6 -> return x5
    | x1 :: x2 :: x3 :: x4 :: [] ->
      _sort_4 x1 x2 x3 x4 @@ fun _x5 x6 _x7 _x8 -> return x6
    | x1 :: x2 :: x3 :: x4 :: x5 :: [] ->
      _sort_5 x1 x2 x3 x4 x5 @@ fun _x6 _x7 x8 _x9 _x10 -> return x8
    | _ ->
      _median_of_fives xs @@ fun medians ->
      _median_of_medians medians return
  in
  let _partition xs pivot return =
    let rec _visit xs k left right =
      match xs with
      | [] -> return k left right
      | x :: xs1 ->
        if _ordered x pivot
        then _visit xs1 (k + 1) (x :: left) right
        else _visit xs1 k left (x :: right)
    in
    _visit xs 0 [] []
  in
  let rec _quick n xs return =
    match xs with
    | [] -> assert false (* Invariant *)
    | x1 :: [] ->
      begin match n with
      | 0 -> return x1
      | _ -> assert false (* Invariant *)
      end
    | x1 :: x2 :: [] ->
      _sort_2 x1 x2 @@ fun x3 x4 ->
      begin match n with
      | 0 -> return x3
      | 1 -> return x4
      | _ -> assert false (* Invariant *)
      end
    | x1 :: x2 :: x3 :: [] ->
      _sort_3 x1 x2 x3 @@ fun x4 x5 x6 ->
      begin match n with
      | 0 -> return x4
      | 1 -> return x5
      | 2 -> return x6
      | _ -> assert false (* Invariant *)
      end
    | x1 :: x2 :: x3 :: x4 :: [] ->
      _sort_4 x1 x2 x3 x4 @@ fun x5 x6 x7 x8 ->
      begin match n with
      | 0 -> return x5
      | 1 -> return x6
      | 2 -> return x7
      | 3 -> return x8
      | _ -> assert false (* Invariant *)
      end
    | x1 :: x2 :: x3 :: x4 :: x5 :: [] ->
      _sort_5 x1 x2 x3 x4 x5 @@ fun x6 x7 x8 x9 x10 ->
      begin match n with
      | 0 -> return x6
      | 1 -> return x7
      | 2 -> return x8
      | 3 -> return x9
      | 4 -> return x10
      | _ -> assert false (* Invariant *)
      end
    | _ ->
      _median_of_medians xs @@ fun pivot ->
      _partition xs pivot @@ fun k left right ->
      length xs |> fun l ->
      if k = l then return pivot else
      if n < k then _quick n left return else
      _quick (n - k) right return
  in
  _quick n xs identity

let sort order xs =
  let _ordered x1 x2 =
    let open Order in
    match order x1 x2 with
    | LT | EQ -> true
    | GT -> false
  in
  let _sort_2 x1 x2 return =
    if _ordered x1 x2
    then return x1 x2
    else return x2 x1
  in
  let _sort_3 x1 x2 x3 return =
    _sort_2 x1 x2 @@ fun x4 x5 ->
    if _ordered x3 x4 then return x3 x4 x5 else
    if _ordered x3 x5 then return x4 x3 x5 else
    return x4 x5 x3
  in
  let _sort_4 x1 x2 x3 x4 return =
    _sort_3 x1 x2 x3 @@ fun x5 x6 x7 ->
    if _ordered x4 x5 then return x4 x5 x6 x7 else
    if _ordered x4 x6 then return x5 x4 x6 x7 else
    if _ordered x4 x7 then return x5 x6 x4 x7 else
    return x5 x6 x7 x4
  in
  let _sort_5 x1 x2 x3 x4 x5 return =
    _sort_4 x1 x2 x3 x4 @@ fun x6 x7 x8 x9 ->
    if _ordered x7 x5 then
      if _ordered x5 x8 then return x6 x7 x5 x8 x9 else
      if _ordered x5 x9 then return x6 x7 x8 x5 x9 else
      return x6 x7 x8 x9 x5
    else
      if _ordered x6 x5
      then return x6 x5 x7 x8 x9
      else return x5 x6 x7 x8 x9
  in
  let _partition xs pivot return =
    let rec _visit xs ln rn left right =
      match xs with
      | [] -> return ln rn left right
      | x :: xs1 ->
        if _ordered x pivot
        then _visit xs1 (ln + 1) rn (x :: left) right
        else _visit xs1 ln (rn + 1) left (x :: right)
    in
    _visit xs 0 0 [] []
  in
  let rec _quick xs n result return =
    match xs with
    | [] -> return result
    | x1 :: [] -> return (x1 :: result)
    | x1 :: x2 :: [] ->
      _sort_2 x1 x2 @@ fun x3 x4 ->
      return (x3 :: x4 :: result)
    | x1 :: x2 :: x3 :: [] ->
      _sort_3 x1 x2 x3 @@ fun x4 x5 x6 ->
      return (x4 :: x5 :: x6 :: result)
    | x1 :: x2 :: x3 :: x4 :: [] ->
      _sort_4 x1 x2 x3 x4 @@ fun x5 x6 x7 x8 ->
      return (x5 :: x6 :: x7 :: x8 :: result)
    | x1 :: x2 :: x3 :: x4 :: x5 :: [] ->
      _sort_5 x1 x2 x3 x4 x5 @@ fun x6 x7 x8 x9 x10 ->
      return (x6 :: x7 :: x8 :: x9 :: x10 :: result)
    | _ ->
      select order (n / 2) xs |> fun pivot ->
      _partition xs pivot @@ fun ln rn left right ->
      length xs |> fun l ->
      if ln = l then return xs else
      _quick right rn result @@ fun result1 ->
      _quick left ln result1 return
  in
  length xs |> fun l ->
  _quick xs l [] identity

let sort_unique order xs =
  let open Order in
  let rec _visit x xs return =
    match xs with
    | [] -> return [ x ]
    | y :: xs1 ->
      match order x y with
      | GT -> assert false (* Invariant *)
      | LT -> _visit y xs1 (return <== (cons x))
      | EQ -> _visit x xs1 return
  in
  sort order xs |> fun xs1 ->
  match xs1 with
  | [] -> []
  | x :: xs2 ->
    _visit x xs2 identity
