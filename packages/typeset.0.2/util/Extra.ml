let identity x = x
let (<==) f g x = f (g x)
let swap f x y = f y x

module List = struct
  let nil = []
  let cons x xs = x :: xs

  let length = List.length

  let fold null_case list_case term =
    let rec _visit xs return =
      match xs with
      | [] -> return null_case
      | x :: xs' ->
        _visit xs' (return <== (list_case x))
    in
    _visit term identity

  let map f term =
    fold [] (cons <== f) term
end
