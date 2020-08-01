let rec topsort ~gt = function
  | [] -> []
  | x :: xs ->
    (match List.partition (gt x) xs with
    | [], xs -> x :: topsort ~gt xs
    | xs, xs' -> topsort ~gt (xs @ [ x ] @ xs'))
;;
