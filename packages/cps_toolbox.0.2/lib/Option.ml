let none = None
let some x = Some x

let map f x =
  match x with
  | Some x' -> Some (f x')
  | _ -> None

let map2 f x y =
  match x, y with
  | Some x', Some y' -> Some (f x' y')
  | _, _ -> None
